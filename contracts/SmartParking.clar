;; SmartParking Contract
;; Decentralized parking management system for cities

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant HOURLY-RATE u10) ;; 10 STX per hour
(define-constant PREMIUM-MULTIPLIER u2) ;; Premium spots cost 2x
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PARKING-SPOT (err u101))
(define-constant ERR-SPOT-OCCUPIED (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-NO-ACTIVE-BOOKING (err u104))

;; Data Variables
(define-data-var total-spots uint u100)
(define-data-var total-revenue uint u0)


(define-constant ERR-VIOLATION-NOT-FOUND (err u111))
(define-constant ERR-FINE-ALREADY-PAID (err u112))
(define-constant ERR-DISPUTE-PERIOD-EXPIRED (err u113))
(define-constant ERR-INVALID-DISPUTE (err u114))

(define-constant OVERSTAY-FINE u30)
(define-constant UNAUTHORIZED-PARKING-FINE u50)
(define-constant EMERGENCY-ZONE-FINE u100)
(define-constant DISPUTE-PERIOD u144)

(define-data-var violation-counter uint u0)
(define-data-var total-fines-collected uint u0)

(define-map parking-violations uint 
    {
        violator: principal,
        spot-id: uint,
        violation-type: (string-ascii 20),
        fine-amount: uint,
        issued-at: uint,
        paid: bool,
        disputed: bool,
        dispute-resolved: bool
    }
)

(define-map emergency-zones uint bool)

(define-map violation-disputes uint 
    {
        violation-id: uint,
        dispute-reason: (string-ascii 100),
        submitted-at: uint,
        reviewed: bool,
        upheld: bool
    }
)

(define-map user-violation-history principal 
    {
        total-violations: uint,
        total-fines-paid: uint,
        repeat-offender: bool
    }
)

(define-map parking-spots uint 
    {
        is-premium: bool,
        is-occupied: bool,
        current-user: (optional principal),
        booking-start: (optional uint),
        booking-duration: (optional uint)
    }
)

(define-map user-bookings principal 
    {
        active-spot: (optional uint),
        total-bookings: uint,
        premium-member: bool
    }
)

;; Public Functions

;; Initialize parking spot
(define-public (initialize-parking-spot (spot-id uint) (is-premium bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (map-set parking-spots spot-id {
            is-premium: is-premium,
            is-occupied: false,
            current-user: none,
            booking-start: none,
            booking-duration: none
        }))
    )
)

;; Book parking spot
(define-public (book-spot (spot-id uint) (duration uint))
    (let (
        (spot (unwrap! (map-get? parking-spots spot-id) ERR-INVALID-PARKING-SPOT))
        (rate (if (get is-premium spot) (* HOURLY-RATE PREMIUM-MULTIPLIER) HOURLY-RATE))
        (total-cost (* rate duration))
    )
        (asserts! (not (get is-occupied spot)) ERR-SPOT-OCCUPIED)
        (try! (stx-transfer? total-cost tx-sender CONTRACT-OWNER))
        
        ;; Update parking spot
        (map-set parking-spots spot-id {
            is-premium: (get is-premium spot),
            is-occupied: true,
            current-user: (some tx-sender),
            booking-start: (some stacks-block-height),
            booking-duration: (some duration)
        })
        
        ;; Update user bookings
        (match (map-get? user-bookings tx-sender)
            prev-booking (map-set user-bookings tx-sender {
                active-spot: (some spot-id),
                total-bookings: (+ u1 (get total-bookings prev-booking)),
                premium-member: (get premium-member prev-booking)
            })
            (map-set user-bookings tx-sender {
                active-spot: (some spot-id),
                total-bookings: u1,
                premium-member: false
            })
        )
        
        (var-set total-revenue (+ (var-get total-revenue) total-cost))
        (ok true)
    )
)

;; End parking session
(define-public (end-parking-session (spot-id uint))
    (let (
        (spot (unwrap! (map-get? parking-spots spot-id) ERR-INVALID-PARKING-SPOT))
    )
        (asserts! (is-eq (some tx-sender) (get current-user spot)) ERR-NOT-AUTHORIZED)
        
        (map-set parking-spots spot-id {
            is-premium: (get is-premium spot),
            is-occupied: false,
            current-user: none,
            booking-start: none,
            booking-duration: none
        })
        
        (match (map-get? user-bookings tx-sender)
            prev-booking
            (map-set user-bookings tx-sender {
                active-spot: none,
                total-bookings: (get total-bookings prev-booking),
                premium-member: (get premium-member prev-booking)
            })
            (map-set user-bookings tx-sender {
                active-spot: none,
                total-bookings: u0,
                premium-member: false
            })
        )
        (ok true)
    )
)

;; Upgrade to premium membership
(define-public (upgrade-to-premium)
    (begin
        (match (map-get? user-bookings tx-sender)
            prev-booking (map-set user-bookings tx-sender {
                active-spot: (get active-spot prev-booking),
                total-bookings: (get total-bookings prev-booking),
                premium-member: true
            })
            (map-set user-bookings tx-sender {
                active-spot: none,
                total-bookings: u0,
                premium-member: true
            })
        )
        (ok true)
    )
)

;; Read-only Functions

(define-read-only (get-spot-details (spot-id uint))
    (map-get? parking-spots spot-id)
)

(define-read-only (get-user-details (user principal))
    (map-get? user-bookings user)
)

(define-read-only (get-total-revenue)
    (var-get total-revenue)
)

(define-read-only (is-spot-available (spot-id uint))
    (match (map-get? parking-spots spot-id)
        spot (not (get is-occupied spot))
        false
    )
)

;; Add to Constants
(define-constant ERR-INVALID-RESERVATION-TIME (err u105))
(define-constant ERR-RESERVATION-EXISTS (err u106))

;; Add to Data Maps
(define-map spot-reservations uint 
    {
        reserved-by: principal,
        start-block: uint,
        duration: uint
    }
)

;; New Public Function
(define-public (reserve-spot (spot-id uint) (start-block uint) (duration uint))
    (let (
        (spot (unwrap! (map-get? parking-spots spot-id) ERR-INVALID-PARKING-SPOT))
        (current-block stacks-block-height)
        (rate (if (get is-premium spot) (* HOURLY-RATE PREMIUM-MULTIPLIER) HOURLY-RATE))
        (total-cost (* rate duration))
        (reservation-fee (/ total-cost u2)) ;; 50% upfront for reservation
    )
        (asserts! (> start-block current-block) ERR-INVALID-RESERVATION-TIME)
        (asserts! (is-none (map-get? spot-reservations spot-id)) ERR-RESERVATION-EXISTS)
        (try! (stx-transfer? reservation-fee tx-sender CONTRACT-OWNER))
        
        (map-set spot-reservations spot-id {
            reserved-by: tx-sender,
            start-block: start-block,
            duration: duration
        })
        
        (var-set total-revenue (+ (var-get total-revenue) reservation-fee))
        (ok true)
    )
)

;; New Read-only Function
(define-read-only (get-spot-reservation (spot-id uint))
    (map-get? spot-reservations spot-id)
)


;; Add to Data Variables
(define-data-var loyalty-threshold uint u10) ;; Number of bookings to get discount
(define-data-var loyalty-discount uint u20) ;; 20% discount

;; Add to Data Maps
(define-map loyalty-points principal uint)

;; New Public Function
(define-public (claim-loyalty-discount (spot-id uint) (duration uint))
    (let (
        (spot (unwrap! (map-get? parking-spots spot-id) ERR-INVALID-PARKING-SPOT))
        (user-booking (unwrap! (map-get? user-bookings tx-sender) ERR-NO-ACTIVE-BOOKING))
        (user-points (default-to u0 (map-get? loyalty-points tx-sender)))
        (rate (if (get is-premium spot) (* HOURLY-RATE PREMIUM-MULTIPLIER) HOURLY-RATE))
        (discount-rate (- u100 (var-get loyalty-discount)))
        (discounted-rate (/ (* rate discount-rate) u100))
        (total-cost (* discounted-rate duration))
    )
        (asserts! (not (get is-occupied spot)) ERR-SPOT-OCCUPIED)
        (asserts! (>= (get total-bookings user-booking) (var-get loyalty-threshold)) ERR-NOT-AUTHORIZED)
        
        (try! (stx-transfer? total-cost tx-sender CONTRACT-OWNER))
        
        ;; Update parking spot
        (map-set parking-spots spot-id {
            is-premium: (get is-premium spot),
            is-occupied: true,
            current-user: (some tx-sender),
            booking-start: (some stacks-block-height),
            booking-duration: (some duration)
        })
        
        ;; Update user bookings
        (map-set user-bookings tx-sender {
            active-spot: (some spot-id),
            total-bookings: (+ u1 (get total-bookings user-booking)),
            premium-member: (get premium-member user-booking)
        })
        
        ;; Update loyalty points
        (map-set loyalty-points tx-sender (+ user-points u1))
        
        (var-set total-revenue (+ (var-get total-revenue) total-cost))
        (ok true)
    )
)

;; New Read-only Function
(define-read-only (get-user-loyalty-points (user principal))
    (default-to u0 (map-get? loyalty-points user))
)


;; Add to Constants
(define-constant ERR-INVALID-EXTENSION (err u107))

;; New Public Function
(define-public (extend-parking-duration (spot-id uint) (additional-hours uint))
    (let (
        (spot (unwrap! (map-get? parking-spots spot-id) ERR-INVALID-PARKING-SPOT))
        (rate (if (get is-premium spot) (* HOURLY-RATE PREMIUM-MULTIPLIER) HOURLY-RATE))
        (additional-cost (* rate additional-hours))
    )
        (asserts! (is-eq (some tx-sender) (get current-user spot)) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (get booking-duration spot)) ERR-NO-ACTIVE-BOOKING)
        
        (try! (stx-transfer? additional-cost tx-sender CONTRACT-OWNER))
        
        (map-set parking-spots spot-id {
            is-premium: (get is-premium spot),
            is-occupied: true,
            current-user: (get current-user spot),
            booking-start: (get booking-start spot),
            booking-duration: (some (+ (unwrap! (get booking-duration spot) ERR-INVALID-EXTENSION) additional-hours))
        })
        
        (var-set total-revenue (+ (var-get total-revenue) additional-cost))
        (ok true)
    )
)


;; Add to Constants
(define-constant EMERGENCY-FEE u50) ;; 50 STX for emergency service
(define-constant ERR-NO-EMERGENCY-AVAILABLE (err u108))

;; Add to Data Variables
(define-data-var emergency-services-available bool true)

;; Add to Data Maps
(define-map emergency-requests uint 
    {
        user: principal,
        service-type: (string-ascii 20),
        block-requested: uint,
        resolved: bool
    }
)

(define-data-var emergency-request-counter uint u0)

;; New Public Function
(define-public (request-emergency-service (spot-id uint) (service-type (string-ascii 20)))
    (let (
        (spot (unwrap! (map-get? parking-spots spot-id) ERR-INVALID-PARKING-SPOT))
        (request-id (var-get emergency-request-counter))
    )
        (asserts! (is-eq (some tx-sender) (get current-user spot)) ERR-NOT-AUTHORIZED)
        (asserts! (var-get emergency-services-available) ERR-NO-EMERGENCY-AVAILABLE)
        
        (try! (stx-transfer? EMERGENCY-FEE tx-sender CONTRACT-OWNER))
        
        (map-set emergency-requests request-id {
            user: tx-sender,
            service-type: service-type,
            block-requested: stacks-block-height,
            resolved: false
        })
        
        (var-set emergency-request-counter (+ (var-get emergency-request-counter) u1))
        (var-set total-revenue (+ (var-get total-revenue) EMERGENCY-FEE))
        (ok request-id)
    )
)

;; New Admin Function
(define-public (resolve-emergency-request (request-id uint))
    (let (
        (request (unwrap! (map-get? emergency-requests request-id) ERR-INVALID-PARKING-SPOT))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (map-set emergency-requests request-id {
            user: (get user request),
            service-type: (get service-type request),
            block-requested: (get block-requested request),
            resolved: true
        })
        
        (ok true)
    )
)

;; New Read-only Function
(define-read-only (get-emergency-request (request-id uint))
    (map-get? emergency-requests request-id)
)


(define-constant ERR-NOT-VALET (err u109))
(define-constant ERR-NO-VALET-REQUEST (err u110))
(define-constant VALET-FEE u25)

(define-map certified-valets principal bool)

(define-map valet-requests uint 
    {
        user: principal,
        valet: (optional principal),
        spot-id: uint,
        status: (string-ascii 10),
        requested-at: uint
    }
)

(define-data-var valet-request-counter uint u0)

(define-public (register-valet (valet-address principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (map-set certified-valets valet-address true))
    )
)

(define-public (request-valet-service (spot-id uint))
    (let (
        (request-id (var-get valet-request-counter))
    )
        (try! (stx-transfer? VALET-FEE tx-sender CONTRACT-OWNER))
        
        (map-set valet-requests request-id {
            user: tx-sender,
            valet: none,
            spot-id: spot-id,
            status: "pending",
            requested-at: stacks-block-height
        })
        
        (var-set valet-request-counter (+ request-id u1))
        (ok request-id)
    )
)

(define-public (accept-valet-request (request-id uint))
    (let (
        (request (unwrap! (map-get? valet-requests request-id) ERR-NO-VALET-REQUEST))
    )
        (asserts! (default-to false (map-get? certified-valets tx-sender)) ERR-NOT-VALET)
        
        (ok (map-set valet-requests request-id {
            user: (get user request),
            valet: (some tx-sender),
            spot-id: (get spot-id request),
            status: "accepted",
            requested-at: (get requested-at request)
        }))
    )
)


(define-public (complete-valet-service (request-id uint))
    (let (
        (request (unwrap! (map-get? valet-requests request-id) ERR-NO-VALET-REQUEST))
    )
        (asserts! (is-eq tx-sender (unwrap! (get valet request) ERR-NOT-VALET)) ERR-NOT-AUTHORIZED)
        
        (map-set valet-requests request-id {
            user: (get user request),
            valet: none,
            spot-id: (get spot-id request),
            status: "completed",
            requested-at: (get requested-at request)
        })
        
        (ok true)
    )
)


(define-constant BASE-SURGE-MULTIPLIER u100)
(define-constant MAX-SURGE-MULTIPLIER u300)
(define-constant OCCUPANCY-THRESHOLD u80)
(define-constant TOTAL-SPOTS u100)

;; Helper function to get minimum of two numbers
(define-private (get-min (a uint) (b uint))
    (if (<= a b) a b))

(define-data-var current-surge-multiplier uint u100)
(define-data-var occupied-spots uint u0)

(define-public (update-surge-pricing)
    (let (
        (occupancy-rate (/ (* (var-get occupied-spots) u100) TOTAL-SPOTS))
        (new-multiplier (if (>= occupancy-rate OCCUPANCY-THRESHOLD)
            (get-min MAX-SURGE-MULTIPLIER (* BASE-SURGE-MULTIPLIER u2))
            BASE-SURGE-MULTIPLIER
        ))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set current-surge-multiplier new-multiplier)
        (ok new-multiplier)
    )
)

(define-read-only (get-current-price (spot-id uint))
    (let (
        (spot (unwrap! (map-get? parking-spots spot-id) ERR-INVALID-PARKING-SPOT))
        (base-rate (if (get is-premium spot) (* HOURLY-RATE PREMIUM-MULTIPLIER) HOURLY-RATE))
    )
        (ok (/ (* base-rate (var-get current-surge-multiplier)) u100))
    )
)


(define-public (designate-emergency-zone (spot-id uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (map-set emergency-zones spot-id true))
    )
)

(define-public (issue-violation (violator principal) (spot-id uint) (violation-type (string-ascii 20)))
    (let (
        (violation-id (var-get violation-counter))
        (fine-amount (get-fine-amount violation-type spot-id))
        (user-history (default-to {total-violations: u0, total-fines-paid: u0, repeat-offender: false} 
                                 (map-get? user-violation-history violator)))
        (is-repeat (>= (get total-violations user-history) u3))
        (final-fine (if is-repeat (* fine-amount u2) fine-amount))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (map-set parking-violations violation-id {
            violator: violator,
            spot-id: spot-id,
            violation-type: violation-type,
            fine-amount: final-fine,
            issued-at: stacks-block-height,
            paid: false,
            disputed: false,
            dispute-resolved: false
        })
        
        (map-set user-violation-history violator {
            total-violations: (+ (get total-violations user-history) u1),
            total-fines-paid: (get total-fines-paid user-history),
            repeat-offender: is-repeat
        })
        
        (var-set violation-counter (+ violation-id u1))
        (ok violation-id)
    )
)

(define-public (pay-fine (violation-id uint))
    (let (
        (violation (unwrap! (map-get? parking-violations violation-id) ERR-VIOLATION-NOT-FOUND))
        (user-history (default-to {total-violations: u0, total-fines-paid: u0, repeat-offender: false} 
                                 (map-get? user-violation-history tx-sender)))
    )
        (asserts! (is-eq tx-sender (get violator violation)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get paid violation)) ERR-FINE-ALREADY-PAID)
        
        (try! (stx-transfer? (get fine-amount violation) tx-sender CONTRACT-OWNER))
        
        (map-set parking-violations violation-id {
            violator: (get violator violation),
            spot-id: (get spot-id violation),
            violation-type: (get violation-type violation),
            fine-amount: (get fine-amount violation),
            issued-at: (get issued-at violation),
            paid: true,
            disputed: (get disputed violation),
            dispute-resolved: (get dispute-resolved violation)
        })
        
        (map-set user-violation-history tx-sender {
            total-violations: (get total-violations user-history),
            total-fines-paid: (+ (get total-fines-paid user-history) (get fine-amount violation)),
            repeat-offender: (get repeat-offender user-history)
        })
        
        (var-set total-fines-collected (+ (var-get total-fines-collected) (get fine-amount violation)))
        (ok true)
    )
)

(define-public (dispute-violation (violation-id uint) (reason (string-ascii 100)))
    (let (
        (violation (unwrap! (map-get? parking-violations violation-id) ERR-VIOLATION-NOT-FOUND))
        (current-block stacks-block-height)
        (dispute-deadline (+ (get issued-at violation) DISPUTE-PERIOD))
    )
        (asserts! (is-eq tx-sender (get violator violation)) ERR-NOT-AUTHORIZED)
        (asserts! (<= current-block dispute-deadline) ERR-DISPUTE-PERIOD-EXPIRED)
        (asserts! (not (get disputed violation)) ERR-INVALID-DISPUTE)
        (asserts! (not (get paid violation)) ERR-FINE-ALREADY-PAID)
        
        (map-set parking-violations violation-id {
            violator: (get violator violation),
            spot-id: (get spot-id violation),
            violation-type: (get violation-type violation),
            fine-amount: (get fine-amount violation),
            issued-at: (get issued-at violation),
            paid: false,
            disputed: true,
            dispute-resolved: false
        })
        
        (map-set violation-disputes violation-id {
            violation-id: violation-id,
            dispute-reason: reason,
            submitted-at: current-block,
            reviewed: false,
            upheld: false
        })
        
        (ok true)
    )
)

(define-public (resolve-dispute (violation-id uint) (uphold-violation bool))
    (let (
        (violation (unwrap! (map-get? parking-violations violation-id) ERR-VIOLATION-NOT-FOUND))
        (dispute (unwrap! (map-get? violation-disputes violation-id) ERR-INVALID-DISPUTE))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (get disputed violation) ERR-INVALID-DISPUTE)
        (asserts! (not (get reviewed dispute)) ERR-INVALID-DISPUTE)
        
        (map-set violation-disputes violation-id {
            violation-id: violation-id,
            dispute-reason: (get dispute-reason dispute),
            submitted-at: (get submitted-at dispute),
            reviewed: true,
            upheld: uphold-violation
        })
        
        (map-set parking-violations violation-id {
            violator: (get violator violation),
            spot-id: (get spot-id violation),
            violation-type: (get violation-type violation),
            fine-amount: (if uphold-violation (get fine-amount violation) u0),
            issued-at: (get issued-at violation),
            paid: (not uphold-violation),
            disputed: true,
            dispute-resolved: true
        })
        
        (ok uphold-violation)
    )
)

(define-public (check-overstay-violations)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok true)
    )
)

;; Private Functions

(define-private (get-fine-amount (violation-type (string-ascii 20)) (spot-id uint))
    (if (is-eq violation-type "overstay")
        OVERSTAY-FINE
        (if (is-eq violation-type "unauthorized")
            UNAUTHORIZED-PARKING-FINE
            (if (is-eq violation-type "emergency-zone")
                EMERGENCY-ZONE-FINE
                u25
            )
        )
    )
)

;; Read-only Functions

(define-read-only (get-violation-details (violation-id uint))
    (map-get? parking-violations violation-id)
)

(define-read-only (get-user-violations (user principal))
    (map-get? user-violation-history user)
)

(define-read-only (get-dispute-details (violation-id uint))
    (map-get? violation-disputes violation-id)
)

(define-read-only (is-emergency-zone (spot-id uint))
    (default-to false (map-get? emergency-zones spot-id))
)

(define-read-only (get-total-fines-collected)
    (var-get total-fines-collected)
)

(define-read-only (get-unpaid-violations (user principal))
    (let (
        (user-history (map-get? user-violation-history user))
    )
        (match user-history
            history (some (get total-violations history))
            none
        )
    )
)