;; Dynamic Parking Auctions Contract
;; Allows users to bid on premium parking spots during high-demand periods

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-AUCTION-NOT-FOUND (err u301))
(define-constant ERR-AUCTION-ENDED (err u302))
(define-constant ERR-AUCTION-NOT-ENDED (err u303))
(define-constant ERR-BID-TOO-LOW (err u304))
(define-constant ERR-CANNOT-BID-OWN-AUCTION (err u305))
(define-constant ERR-REFUND-FAILED (err u306))
(define-constant ERR-NO-BIDS (err u307))
(define-constant ERR-AUCTION-ACTIVE (err u308))
(define-constant ERR-SPOT-NOT-AVAILABLE (err u309))
(define-constant ERR-INSUFFICIENT-FUNDS (err u310))

;; Configuration constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-BID-INCREMENT u5) ;; Minimum 5 STX increase per bid
(define-constant AUCTION-DURATION u72) ;; 72 blocks (~12 hours)
(define-constant MIN-STARTING-BID u20) ;; Minimum starting bid 20 STX
(define-constant AUCTION-FEE-PERCENT u5) ;; 5% platform fee

;; Data variables
(define-data-var auction-counter uint u0)
(define-data-var total-auction-revenue uint u0)
(define-data-var active-auctions-count uint u0)

;; Main auction data structure
(define-map parking-auctions uint 
    {
        spot-id: uint,
        seller: principal, ;; Contract owner or spot owner
        start-block: uint,
        end-block: uint,
        duration-hours: uint, ;; Parking duration being auctioned
        starting-bid: uint,
        current-highest-bid: uint,
        highest-bidder: (optional principal),
        total-bids: uint,
        status: (string-ascii 12), ;; "active", "ended", "cancelled"
        winner-claimed: bool
    }
)

;; Individual bid tracking
(define-map auction-bids { auction-id: uint, bidder: principal } 
    {
        bid-amount: uint,
        bid-time: uint,
        refunded: bool
    }
)

;; User auction participation history
(define-map user-auction-stats principal 
    {
        total-bids-placed: uint,
        total-auctions-won: uint,
        total-spent: uint,
        average-winning-bid: uint
    }
)

;; Track all bids for an auction (for refund purposes)
(define-map auction-bid-list uint (list 50 principal))

;; Public functions

;; Create a new auction for a parking spot
(define-public (create-auction (spot-id uint) (duration-hours uint) (starting-bid uint))
    (let (
        (auction-id (var-get auction-counter))
        (current-block stacks-block-height)
        (end-block (+ current-block AUCTION-DURATION))
    )
        ;; Only contract owner can create auctions for now
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (>= starting-bid MIN-STARTING-BID) ERR-BID-TOO-LOW)
        
        ;; Create the auction
        (map-set parking-auctions auction-id {
            spot-id: spot-id,
            seller: tx-sender,
            start-block: current-block,
            end-block: end-block,
            duration-hours: duration-hours,
            starting-bid: starting-bid,
            current-highest-bid: starting-bid,
            highest-bidder: none,
            total-bids: u0,
            status: "active",
            winner-claimed: false
        })
        
        ;; Initialize empty bid list
        (map-set auction-bid-list auction-id (list))
        
        ;; Update counters
        (var-set auction-counter (+ auction-id u1))
        (var-set active-auctions-count (+ (var-get active-auctions-count) u1))
        
        (ok auction-id)
    )
)

;; Place a bid on an active auction
(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let (
        (auction (unwrap! (map-get? parking-auctions auction-id) ERR-AUCTION-NOT-FOUND))
        (current-block stacks-block-height)
        (current-highest (get current-highest-bid auction))
        (min-required-bid (+ current-highest MIN-BID-INCREMENT))
        (bidder-list (default-to (list) (map-get? auction-bid-list auction-id)))
        (updated-bidder-list (unwrap! (as-max-len? (append bidder-list tx-sender) u50) ERR-NO-BIDS))
    )
        ;; Validation checks
        (asserts! (is-eq (get status auction) "active") ERR-AUCTION-ENDED)
        (asserts! (< current-block (get end-block auction)) ERR-AUCTION-ENDED)
        (asserts! (not (is-eq tx-sender (get seller auction))) ERR-CANNOT-BID-OWN-AUCTION)
        (asserts! (>= bid-amount min-required-bid) ERR-BID-TOO-LOW)
        
        ;; Transfer bid amount to contract
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        
        ;; Store the bid
        (map-set auction-bids { auction-id: auction-id, bidder: tx-sender } {
            bid-amount: bid-amount,
            bid-time: current-block,
            refunded: false
        })
        
        ;; Update auction with new highest bid
        (map-set parking-auctions auction-id {
            spot-id: (get spot-id auction),
            seller: (get seller auction),
            start-block: (get start-block auction),
            end-block: (get end-block auction),
            duration-hours: (get duration-hours auction),
            starting-bid: (get starting-bid auction),
            current-highest-bid: bid-amount,
            highest-bidder: (some tx-sender),
            total-bids: (+ (get total-bids auction) u1),
            status: "active",
            winner-claimed: false
        })
        
        ;; Update bidder list
        (map-set auction-bid-list auction-id updated-bidder-list)
        
        ;; Update user stats
        (update-user-bid-stats tx-sender bid-amount)
        
        (ok true)
    )
)

;; End auction and determine winner
(define-public (end-auction (auction-id uint))
    (let (
        (auction (unwrap! (map-get? parking-auctions auction-id) ERR-AUCTION-NOT-FOUND))
        (current-block stacks-block-height)
    )
        ;; Validation
        (asserts! (>= current-block (get end-block auction)) ERR-AUCTION-NOT-ENDED)
        (asserts! (is-eq (get status auction) "active") ERR-AUCTION-ENDED)
        
        ;; Update auction status
        (map-set parking-auctions auction-id {
            spot-id: (get spot-id auction),
            seller: (get seller auction),
            start-block: (get start-block auction),
            end-block: (get end-block auction),
            duration-hours: (get duration-hours auction),
            starting-bid: (get starting-bid auction),
            current-highest-bid: (get current-highest-bid auction),
            highest-bidder: (get highest-bidder auction),
            total-bids: (get total-bids auction),
            status: "ended",
            winner-claimed: false
        })
        
        ;; Decrease active auction count
        (var-set active-auctions-count (- (var-get active-auctions-count) u1))
        
        (ok (get highest-bidder auction))
    )
)

;; Winner claims their parking spot
(define-public (claim-winning-spot (auction-id uint))
    (let (
        (auction (unwrap! (map-get? parking-auctions auction-id) ERR-AUCTION-NOT-FOUND))
        (winner (unwrap! (get highest-bidder auction) ERR-NO-BIDS))
        (winning-bid (get current-highest-bid auction))
        (platform-fee (/ (* winning-bid AUCTION-FEE-PERCENT) u100))
        (seller-amount (- winning-bid platform-fee))
    )
        ;; Validation
        (asserts! (is-eq tx-sender winner) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status auction) "ended") ERR-AUCTION-ACTIVE)
        (asserts! (not (get winner-claimed auction)) ERR-AUCTION-ENDED)
        
        ;; Transfer funds to seller (minus platform fee)
        (try! (as-contract (stx-transfer? seller-amount tx-sender (get seller auction))))
        
        ;; Update auction as claimed
        (map-set parking-auctions auction-id {
            spot-id: (get spot-id auction),
            seller: (get seller auction),
            start-block: (get start-block auction),
            end-block: (get end-block auction),
            duration-hours: (get duration-hours auction),
            starting-bid: (get starting-bid auction),
            current-highest-bid: (get current-highest-bid auction),
            highest-bidder: (get highest-bidder auction),
            total-bids: (get total-bids auction),
            status: "ended",
            winner-claimed: true
        })
        
        ;; Update revenue tracking
        (var-set total-auction-revenue (+ (var-get total-auction-revenue) platform-fee))
        
        ;; Update winner's stats
        (update-user-win-stats tx-sender winning-bid)
        
        (ok {
            spot-id: (get spot-id auction),
            duration-hours: (get duration-hours auction),
            final-price: winning-bid
        })
    )
)

;; Refund losing bidders
(define-public (refund-losing-bid (auction-id uint) (bidder principal))
    (let (
        (auction (unwrap! (map-get? parking-auctions auction-id) ERR-AUCTION-NOT-FOUND))
        (bid-key { auction-id: auction-id, bidder: bidder })
        (bid-data (unwrap! (map-get? auction-bids bid-key) ERR-NO-BIDS))
        (winner (get highest-bidder auction))
    )
        ;; Validation
        (asserts! (is-eq (get status auction) "ended") ERR-AUCTION-ACTIVE)
        (asserts! (not (get refunded bid-data)) ERR-REFUND-FAILED)
        (asserts! (not (is-eq (some bidder) winner)) ERR-NOT-AUTHORIZED)
        
        ;; Process refund
        (try! (as-contract (stx-transfer? (get bid-amount bid-data) tx-sender bidder)))
        
        ;; Mark as refunded
        (map-set auction-bids bid-key {
            bid-amount: (get bid-amount bid-data),
            bid-time: (get bid-time bid-data),
            refunded: true
        })
        
        (ok (get bid-amount bid-data))
    )
)

;; Cancel auction (emergency function)
(define-public (cancel-auction (auction-id uint))
    (let (
        (auction (unwrap! (map-get? parking-auctions auction-id) ERR-AUCTION-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status auction) "active") ERR-AUCTION-ENDED)
        
        (map-set parking-auctions auction-id {
            spot-id: (get spot-id auction),
            seller: (get seller auction),
            start-block: (get start-block auction),
            end-block: (get end-block auction),
            duration-hours: (get duration-hours auction),
            starting-bid: (get starting-bid auction),
            current-highest-bid: (get current-highest-bid auction),
            highest-bidder: (get highest-bidder auction),
            total-bids: (get total-bids auction),
            status: "cancelled",
            winner-claimed: false
        })
        
        (var-set active-auctions-count (- (var-get active-auctions-count) u1))
        (ok true)
    )
)

;; Private helper functions

;; Update user bidding statistics
(define-private (update-user-bid-stats (user principal) (bid-amount uint))
    (let (
        (current-stats (default-to 
            { total-bids-placed: u0, total-auctions-won: u0, total-spent: u0, average-winning-bid: u0 }
            (map-get? user-auction-stats user)
        ))
    )
        (map-set user-auction-stats user {
            total-bids-placed: (+ (get total-bids-placed current-stats) u1),
            total-auctions-won: (get total-auctions-won current-stats),
            total-spent: (get total-spent current-stats),
            average-winning-bid: (get average-winning-bid current-stats)
        })
        true
    )
)

;; Update user winning statistics  
(define-private (update-user-win-stats (user principal) (winning-bid uint))
    (let (
        (current-stats (default-to 
            { total-bids-placed: u0, total-auctions-won: u0, total-spent: u0, average-winning-bid: u0 }
            (map-get? user-auction-stats user)
        ))
        (new-wins (+ (get total-auctions-won current-stats) u1))
        (new-total-spent (+ (get total-spent current-stats) winning-bid))
        (new-average (/ new-total-spent new-wins))
    )
        (map-set user-auction-stats user {
            total-bids-placed: (get total-bids-placed current-stats),
            total-auctions-won: new-wins,
            total-spent: new-total-spent,
            average-winning-bid: new-average
        })
        true
    )
)

;; Read-only functions

;; Get auction details
(define-read-only (get-auction-details (auction-id uint))
    (map-get? parking-auctions auction-id)
)

;; Get bid details for specific bidder
(define-read-only (get-bid-details (auction-id uint) (bidder principal))
    (map-get? auction-bids { auction-id: auction-id, bidder: bidder })
)

;; Get user auction statistics
(define-read-only (get-user-auction-stats (user principal))
    (map-get? user-auction-stats user)
)

;; Check if auction is active
(define-read-only (is-auction-active (auction-id uint))
    (match (map-get? parking-auctions auction-id)
        auction (and 
            (is-eq (get status auction) "active")
            (< stacks-block-height (get end-block auction))
        )
        false
    )
)

;; Get current highest bid info
(define-read-only (get-current-winning-bid (auction-id uint))
    (match (map-get? parking-auctions auction-id)
        auction (some {
            amount: (get current-highest-bid auction),
            bidder: (get highest-bidder auction),
            total-bids: (get total-bids auction)
        })
        none
    )
)

;; Get minimum next bid amount
(define-read-only (get-min-next-bid (auction-id uint))
    (match (map-get? parking-auctions auction-id)
        auction (some (+ (get current-highest-bid auction) MIN-BID-INCREMENT))
        none
    )
)

;; Get auction time remaining
(define-read-only (get-time-remaining (auction-id uint))
    (match (map-get? parking-auctions auction-id)
        auction (if (> (get end-block auction) stacks-block-height)
            (some (- (get end-block auction) stacks-block-height))
            (some u0)
        )
        none
    )
)

;; Get total platform revenue
(define-read-only (get-total-auction-revenue)
    (var-get total-auction-revenue)
)

;; Get active auctions count
(define-read-only (get-active-auctions-count)
    (var-get active-auctions-count)
)



