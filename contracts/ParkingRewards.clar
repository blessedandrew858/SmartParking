;; Parking Rewards System Contract
;; Incentivizes good parking behavior and rewards frequent users

;; Error constants
(define-constant err-not-authorized (err u500))
(define-constant err-insufficient-points (err u501))
(define-constant err-invalid-reward (err u502))
(define-constant err-reward-not-found (err u503))
(define-constant err-already-redeemed (err u504))
(define-constant err-tier-not-reached (err u505))
(define-constant err-invalid-tier (err u506))

;; Constants
(define-constant contract-owner tx-sender)
(define-constant points-per-booking u10)
(define-constant points-per-hour u2)
(define-constant violation-penalty u50)
(define-constant referral-bonus u25)

;; Reward tier thresholds
(define-constant bronze-tier-threshold u100)
(define-constant silver-tier-threshold u500)
(define-constant gold-tier-threshold u1000)
(define-constant platinum-tier-threshold u2500)

;; Tier names
(define-constant tier-bronze "bronze")
(define-constant tier-silver "silver")
(define-constant tier-gold "gold")
(define-constant tier-platinum "platinum")

;; Data variables
(define-data-var total-points-issued uint u0)
(define-data-var total-points-redeemed uint u0)
(define-data-var reward-id-nonce uint u1)

;; User reward points and tier information
(define-map user-rewards
  principal
  {
    total-points: uint,
    available-points: uint,
    tier: (string-ascii 10),
    bookings-completed: uint,
    violations-avoided: uint,
    referrals-made: uint,
    last-activity: uint
  }
)

;; Available rewards catalog
(define-map reward-catalog
  uint ;; reward-id
  {
    reward-id: uint,
    name: (string-ascii 50),
    description: (string-ascii 100),
    points-cost: uint,
    tier-required: (string-ascii 10),
    reward-type: (string-ascii 20),
    discount-percentage: uint,
    active: bool
  }
)

;; User reward redemption history
(define-map redemption-history
  {user: principal, reward-id: uint}
  {
    redeemed-at: uint,
    points-spent: uint,
    benefit-used: bool,
    expiry-date: uint
  }
)

;; User tier benefits tracking
(define-map tier-benefits
  {user: principal, benefit-type: (string-ascii 20)}
  {
    active: bool,
    uses-remaining: uint,
    activated-at: uint,
    expires-at: uint
  }
)

;; Award points for parking activity
(define-public (award-points (user principal) (activity-type (string-ascii 20)) (bonus-multiplier uint))
  (let
    (
      (current-rewards (default-to 
        {total-points: u0, available-points: u0, tier: "bronze", 
         bookings-completed: u0, violations-avoided: u0, referrals-made: u0, last-activity: u0}
        (map-get? user-rewards user)))
      (points-earned (calculate-points-earned activity-type bonus-multiplier))
      (new-total-points (+ (get total-points current-rewards) points-earned))
      (new-available-points (+ (get available-points current-rewards) points-earned))
      (new-tier (calculate-user-tier new-total-points))
    )
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    
    ;; Update user rewards
    (map-set user-rewards user
      (merge current-rewards {
        total-points: new-total-points,
        available-points: new-available-points,
        tier: new-tier,
        bookings-completed: (if (is-eq activity-type "booking") 
                            (+ (get bookings-completed current-rewards) u1)
                            (get bookings-completed current-rewards)),
        violations-avoided: (if (is-eq activity-type "clean-record")
                           (+ (get violations-avoided current-rewards) u1)
                           (get violations-avoided current-rewards)),
        referrals-made: (if (is-eq activity-type "referral")
                       (+ (get referrals-made current-rewards) u1)
                       (get referrals-made current-rewards)),
        last-activity: stacks-block-height
      })
    )
    
    (var-set total-points-issued (+ (var-get total-points-issued) points-earned))
    (ok points-earned)
  )
)

;; Redeem reward with points
(define-public (redeem-reward (reward-id uint))
  (let
    (
      (reward (unwrap! (map-get? reward-catalog reward-id) err-reward-not-found))
      (user-rewards-data (unwrap! (map-get? user-rewards tx-sender) err-insufficient-points))
      (points-cost (get points-cost reward))
      (required-tier (get tier-required reward))
      (user-tier (get tier user-rewards-data))
      (redemption-key {user: tx-sender, reward-id: reward-id})
    )
    ;; Validate redemption
    (asserts! (get active reward) err-invalid-reward)
    (asserts! (>= (get available-points user-rewards-data) points-cost) err-insufficient-points)
    (asserts! (tier-meets-requirement user-tier required-tier) err-tier-not-reached)
    (asserts! (is-none (map-get? redemption-history redemption-key)) err-already-redeemed)
    
    ;; Process redemption
    (map-set redemption-history redemption-key
      {
        redeemed-at: stacks-block-height,
        points-spent: points-cost,
        benefit-used: false,
        expiry-date: (+ stacks-block-height u1008) ;; 7 days expiry
      }
    )
    
    ;; Deduct points from user
    (map-set user-rewards tx-sender
      (merge user-rewards-data {
        available-points: (- (get available-points user-rewards-data) points-cost)
      })
    )
    
    ;; Activate tier benefit if applicable
    (if (is-eq (get reward-type reward) "tier-benefit")
      (unwrap! (activate-tier-benefit tx-sender (get reward-type reward)) err-invalid-reward)
      true
    )
    
    (var-set total-points-redeemed (+ (var-get total-points-redeemed) points-cost))
    (ok reward-id)
  )
)

;; Create new reward in catalog (admin only)
(define-public (create-reward
  (name (string-ascii 50))
  (description (string-ascii 100))
  (points-cost uint)
  (tier-required (string-ascii 10))
  (reward-type (string-ascii 20))
  (discount-percentage uint))
  (let
    (
      (reward-id (var-get reward-id-nonce))
    )
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-valid-tier tier-required) err-invalid-tier)
    
    (map-set reward-catalog reward-id
      {
        reward-id: reward-id,
        name: name,
        description: description,
        points-cost: points-cost,
        tier-required: tier-required,
        reward-type: reward-type,
        discount-percentage: discount-percentage,
        active: true
      }
    )
    
    (var-set reward-id-nonce (+ reward-id u1))
    (ok reward-id)
  )
)

;; Activate tier benefit for user
(define-private (activate-tier-benefit (user principal) (benefit-name (string-ascii 20)))
  (let
    (
      (benefit-key {user: user, benefit-type: benefit-name})
      (uses-count (if (is-eq benefit-name "priority-booking") u5
                  (if (is-eq benefit-name "discount-parking") u10 u3)))
    )
    (map-set tier-benefits benefit-key
      {
        active: true,
        uses-remaining: uses-count,
        activated-at: stacks-block-height,
        expires-at: (+ stacks-block-height u4032) ;; 30 days
      }
    )
    (ok true)
  )
)

;; Use tier benefit
(define-public (use-tier-benefit (benefit-type (string-ascii 20)))
  (let
    (
      (benefit-key {user: tx-sender, benefit-type: benefit-type})
      (benefit (unwrap! (map-get? tier-benefits benefit-key) err-invalid-reward))
    )
    (asserts! (get active benefit) err-invalid-reward)
    (asserts! (> (get uses-remaining benefit) u0) err-insufficient-points)
    (asserts! (< stacks-block-height (get expires-at benefit)) err-already-redeemed)
    
    ;; Update benefit usage
    (map-set tier-benefits benefit-key
      (merge benefit {
        uses-remaining: (- (get uses-remaining benefit) u1)
      })
    )
    
    (ok true)
  )
)

;; Helper functions

(define-private (calculate-points-earned (activity-type (string-ascii 20)) (multiplier uint))
  (let
    (
      (base-points (if (is-eq activity-type "booking") points-per-booking
                   (if (is-eq activity-type "hourly") points-per-hour
                   (if (is-eq activity-type "referral") referral-bonus u5))))
    )
    (* base-points multiplier)
  )
)

(define-private (calculate-user-tier (total-points uint))
  (if (>= total-points platinum-tier-threshold) tier-platinum
  (if (>= total-points gold-tier-threshold) tier-gold
  (if (>= total-points silver-tier-threshold) tier-silver tier-bronze)))
)

(define-private (tier-meets-requirement (user-tier (string-ascii 10)) (required-tier (string-ascii 10)))
  (let
    (
      (user-tier-value (tier-to-value user-tier))
      (required-tier-value (tier-to-value required-tier))
    )
    (>= user-tier-value required-tier-value)
  )
)

(define-private (tier-to-value (tier (string-ascii 10)))
  (if (is-eq tier tier-platinum) u4
  (if (is-eq tier tier-gold) u3
  (if (is-eq tier tier-silver) u2 u1)))
)

(define-private (is-valid-tier (tier (string-ascii 10)))
  (or (is-eq tier tier-bronze)
      (or (is-eq tier tier-silver)
          (or (is-eq tier tier-gold)
              (is-eq tier tier-platinum))))
)

;; Read-only functions

(define-read-only (get-user-rewards (user principal))
  (map-get? user-rewards user)
)

(define-read-only (get-reward-details (reward-id uint))
  (map-get? reward-catalog reward-id)
)

(define-read-only (get-user-tier (user principal))
  (match (map-get? user-rewards user)
    rewards (get tier rewards)
    tier-bronze
  )
)

(define-read-only (get-redemption-history (user principal) (reward-id uint))
  (map-get? redemption-history {user: user, reward-id: reward-id})
)

(define-read-only (get-tier-benefit (user principal) (benefit-type (string-ascii 20)))
  (map-get? tier-benefits {user: user, benefit-type: benefit-type})
)

(define-read-only (get-total-points-issued)
  (var-get total-points-issued)
)

(define-read-only (get-total-points-redeemed)
  (var-get total-points-redeemed)
)

(define-read-only (get-last-reward-id)
  (- (var-get reward-id-nonce) u1)
)

(define-read-only (points-to-next-tier (user principal))
  (match (map-get? user-rewards user)
    rewards (let
             (
               (current-points (get total-points rewards))
               (current-tier (get tier rewards))
             )
             (if (is-eq current-tier tier-bronze) (- silver-tier-threshold current-points)
             (if (is-eq current-tier tier-silver) (- gold-tier-threshold current-points)
             (if (is-eq current-tier tier-gold) (- platinum-tier-threshold current-points) u0))))
    silver-tier-threshold
  )
)
