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

