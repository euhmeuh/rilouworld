#lang s-exp "../langs/dimension.rkt"

(dimension "Manafia"
  (zone 'golfia "Golfia" (rect 0 0 256 128)
    (resources
      (larva (monster))
      (home-out-001 (home-out)))
    (monster-nest 'larva (pos 50 60) 0.2)
    (home 'home-out-001 (pos 60 90) 'home-in-001))
  (zone-inside 'home-in-001 "Sample home" (size 100 80)
    (resources
      (sofa-001 (object "props/sofa-001.png" (hitbox (rect 0 20 30 20))))
      (lamp-001 (object "props/lamp-001.png"))
      (desk-001 (object "props/desk-001.png" (hitbox (rect 0 20 30 10)))))
    (props 'sofa-001 (pos 20 40))
    (props 'lamp-001 (pos 50 10))
    (props 'desk-001 (pos 70 40))))
