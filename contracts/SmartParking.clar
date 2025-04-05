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

;; Data Maps
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

