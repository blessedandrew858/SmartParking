;; Group Booking Feature for SmartParking

(define-constant ERR-GROUP-NOT-FOUND (err u200))
(define-constant ERR-GROUP-FULL (err u201))
(define-constant ERR-ALREADY-MEMBER (err u202))
(define-constant ERR-NOT-GROUP-CREATOR (err u203))
(define-constant ERR-GROUP-ACTIVE (err u204))
(define-constant ERR-INVITATION-NOT-FOUND (err u205))
(define-constant ERR-INSUFFICIENT-MEMBERS (err u206))
(define-constant ERR-BOOKING-WINDOW-CLOSED (err u207))

(define-constant MAX-GROUP-SIZE u8)
(define-constant MIN-GROUP-SIZE u2)
(define-constant GROUP-CREATION-FEE u5)
(define-constant BOOKING-WINDOW-BLOCKS u144)

(define-data-var group-counter uint u0)
(define-data-var invitation-counter uint u0)

(define-map group-bookings uint 
    {
        creator: principal,
        target-spots: uint,
        start-block: uint,
        duration: uint,
        cost-per-person: uint,
        members: (list 8 principal),
        status: (string-ascii 10),
        created-at: uint,
        booking-deadline: uint
    }
)

(define-map group-invitations uint 
    {
        group-id: uint,
        invitee: principal,
        invited-by: principal,
        status: (string-ascii 10),
        sent-at: uint
    }
)

(define-map user-group-membership principal uint)

(define-public (create-group-booking (target-spots uint) (start-block uint) (duration uint))
    (let (
        (group-id (var-get group-counter))
        (current-block stacks-block-height)
        (booking-deadline (+ current-block BOOKING-WINDOW-BLOCKS))
        (estimated-cost (* target-spots (* u10 duration)))
        (cost-per-person (/ estimated-cost MIN-GROUP-SIZE))
    )
        (asserts! (> start-block booking-deadline) ERR-BOOKING-WINDOW-CLOSED)
        (asserts! (<= target-spots MAX-GROUP-SIZE) ERR-GROUP-FULL)
        (asserts! (>= target-spots MIN-GROUP-SIZE) ERR-INSUFFICIENT-MEMBERS)
        
        (try! (stx-transfer? GROUP-CREATION-FEE tx-sender (as-contract tx-sender)))
        
        (map-set group-bookings group-id {
            creator: tx-sender,
            target-spots: target-spots,
            start-block: start-block,
            duration: duration,
            cost-per-person: cost-per-person,
            members: (list tx-sender),
            status: "recruiting",
            created-at: current-block,
            booking-deadline: booking-deadline
        })
        
        (map-set user-group-membership tx-sender group-id)
        (var-set group-counter (+ group-id u1))
        (ok group-id)
    )
)

(define-public (invite-to-group (group-id uint) (invitee principal))
    (let (
        (group (unwrap! (map-get? group-bookings group-id) ERR-GROUP-NOT-FOUND))
        (invitation-id (var-get invitation-counter))
        (current-members (get members group))
    )
        (asserts! (is-eq tx-sender (get creator group)) ERR-NOT-GROUP-CREATOR)
        (asserts! (is-eq (get status group) "recruiting") ERR-GROUP-ACTIVE)
        (asserts! (< (len current-members) MAX-GROUP-SIZE) ERR-GROUP-FULL)
        (asserts! (is-none (index-of current-members invitee)) ERR-ALREADY-MEMBER)
        
        (map-set group-invitations invitation-id {
            group-id: group-id,
            invitee: invitee,
            invited-by: tx-sender,
            status: "pending",
            sent-at: stacks-block-height
        })
        
        (var-set invitation-counter (+ invitation-id u1))
        (ok invitation-id)
    )
)

(define-public (join-group (invitation-id uint))
    (let (
        (invitation (unwrap! (map-get? group-invitations invitation-id) ERR-INVITATION-NOT-FOUND))
        (group-id (get group-id invitation))
        (group (unwrap! (map-get? group-bookings group-id) ERR-GROUP-NOT-FOUND))
        (current-members (get members group))
        (updated-members (unwrap! (as-max-len? (append current-members tx-sender) u8) ERR-GROUP-FULL))
    )
        (asserts! (is-eq tx-sender (get invitee invitation)) ERR-NOT-GROUP-CREATOR)
        (asserts! (is-eq (get status invitation) "pending") ERR-INVITATION-NOT-FOUND)
        (asserts! (is-eq (get status group) "recruiting") ERR-GROUP-ACTIVE)
        (asserts! (< (len current-members) MAX-GROUP-SIZE) ERR-GROUP-FULL)
        
        (map-set group-invitations invitation-id {
            group-id: group-id,
            invitee: tx-sender,
            invited-by: (get invited-by invitation),
            status: "accepted",
            sent-at: (get sent-at invitation)
        })
        
        (map-set group-bookings group-id {
            creator: (get creator group),
            target-spots: (get target-spots group),
            start-block: (get start-block group),
            duration: (get duration group),
            cost-per-person: (get cost-per-person group),
            members: updated-members,
            status: (get status group),
            created-at: (get created-at group),
            booking-deadline: (get booking-deadline group)
        })
        
        (map-set user-group-membership tx-sender group-id)
        (ok true)
    )
)

(define-public (finalize-group-booking (group-id uint))
    (let (
        (group (unwrap! (map-get? group-bookings group-id) ERR-GROUP-NOT-FOUND))
        (member-count (len (get members group)))
        (total-cost (* (get target-spots group) (* u10 (get duration group))))
        (updated-cost-per-person (/ total-cost member-count))
    )
        (asserts! (is-eq tx-sender (get creator group)) ERR-NOT-GROUP-CREATOR)
        (asserts! (is-eq (get status group) "recruiting") ERR-GROUP-ACTIVE)
        (asserts! (>= member-count MIN-GROUP-SIZE) ERR-INSUFFICIENT-MEMBERS)
        (asserts! (>= stacks-block-height (get booking-deadline group)) ERR-BOOKING-WINDOW-CLOSED)
        
        (map-set group-bookings group-id {
            creator: (get creator group),
            target-spots: (get target-spots group),
            start-block: (get start-block group),
            duration: (get duration group),
            cost-per-person: updated-cost-per-person,
            members: (get members group),
            status: "finalized",
            created-at: (get created-at group),
            booking-deadline: (get booking-deadline group)
        })
        
        (ok updated-cost-per-person)
    )
)

(define-public (pay-group-share (group-id uint))
    (let (
        (group (unwrap! (map-get? group-bookings group-id) ERR-GROUP-NOT-FOUND))
        (current-members (get members group))
        (payment-amount (get cost-per-person group))
    )
        (asserts! (is-some (index-of current-members tx-sender)) ERR-NOT-GROUP-CREATOR)
        (asserts! (is-eq (get status group) "finalized") ERR-GROUP-ACTIVE)
        
        (try! (stx-transfer? payment-amount tx-sender (as-contract tx-sender)))
        (ok true)
    )
)

(define-public (cancel-group-booking (group-id uint))
    (let (
        (group (unwrap! (map-get? group-bookings group-id) ERR-GROUP-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get creator group)) ERR-NOT-GROUP-CREATOR)
        (asserts! (is-eq (get status group) "recruiting") ERR-GROUP-ACTIVE)
        
        (map-set group-bookings group-id {
            creator: (get creator group),
            target-spots: (get target-spots group),
            start-block: (get start-block group),
            duration: (get duration group),
            cost-per-person: (get cost-per-person group),
            members: (get members group),
            status: "cancelled",
            created-at: (get created-at group),
            booking-deadline: (get booking-deadline group)
        })
        
        (ok true)
    )
)

(define-read-only (get-group-details (group-id uint))
    (map-get? group-bookings group-id)
)

(define-read-only (get-user-group (user principal))
    (map-get? user-group-membership user)
)

(define-read-only (get-invitation-details (invitation-id uint))
    (map-get? group-invitations invitation-id)
)

(define-read-only (get-group-member-count (group-id uint))
    (match (map-get? group-bookings group-id)
        group (some (len (get members group)))
        none
    )
)

(define-read-only (is-group-member (group-id uint) (user principal))
    (match (map-get? group-bookings group-id)
        group (is-some (index-of (get members group) user))
        false
    )
)

(define-read-only (get-available-spots-for-group (group-id uint))
    (match (map-get? group-bookings group-id)
        group (some (- MAX-GROUP-SIZE (len (get members group))))
        none
    )
)
