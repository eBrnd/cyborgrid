sprite_3 ALIGN 64
sprite_3 .byte $07,$ff,$e0, $0f,$ff,$f0, $1f,$ff,$f8, $3c,$00,$3c, $78,$00,$1e, $f0,$00,$0f, $60,$00,$07, $00,$00,$07, $00,$00,$07, $00,$07,$ff, $00,$0f,$fe, $00,$07,$ff, $00,$00,$07, $00,$00,$07, $60,$00,$07, $f0,$00,$0f, $78,$00,$1e, $3c,$00,$3c, $1f,$ff,$f8, $0f,$ff,$f0, $07,$ff,$e0

sprite_2 ALIGN 64
sprite_2 .byte $07,$ff,$e0, $0f,$ff,$f0, $1f,$ff,$f8, $3c,$00,$3c, $78,$00,$1e, $f0,$00,$0f, $60,$00,$07, $00,$00,$07, $00,$00,$07, $00,$0f,$ff, $00,$1f,$fe, $00,$7f,$fc, $00,$f8,$00, $01,$f0,$00, $03,$c0,$00, $0f,$80,$00, $1f,$00,$00, $3c,$00,$00, $7f,$ff,$fe, $ff,$ff,$ff, $7f,$ff,$fe

sprite_1 ALIGN 64
sprite_1 .byte $00,$0c,$00, $00,$1e,$00, $00,$3e,$00, $00,$7e,$00, $00,$fe,$00, $01,$ee,$00, $00,$ce,$00, $00,$0e,$00, $00,$0e,$00, $00,$0e,$00, $00,$0e,$00, $00,$0e,$00, $00,$0e,$00, $00,$0e,$00, $00,$0e,$00, $00,$0e,$00, $00,$0e,$00, $00,$0e,$00, $00,$7f,$c0, $00,$ff,$e0, $00,$7f,$c0

sprite_go ALIGN 64 ; unused for now - does not fit in designated memory area
sprite_go .byte $07,$e3,$fc, $0f,$c6,$fe, $1f,$8e,$ff, $3c,$0e,$0f, $78,$0e,$07, $f0,$0e,$07, $e0,$0e,$07, $e0,$0e,$07, $e0,$0e,$07, $e0,$0e,$07, $e0,$0e,$07, $e7,$ee,$07, $e3,$ee,$07, $e1,$ee,$07, $e0,$ee,$07, $f0,$ee,$07, $78,$ee,$07, $3c,$ef,$07, $1f,$ef,$f7, $0f,$e7,$f6, $07,$e3,$fc
