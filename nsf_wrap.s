;
; NSF Header
;
.segment "HEADER"
;
.byte 'N', 'E', 'S', 'M', $1A				; ID
.byte $01									; Version
.byte 1  									; Number of songs
.byte 1										; Start song
.word $8000									;;; ;; ; LOAD, patched in tracker
.word INIT
.word PLAY
.byte "ft driver                      ", 0	; Name, 32 bytes
.byte "                               ", 0	; Artist, 32 bytes
.byte "                               ", 0	; Copyright, 32 bytes
.word $411A									; NTSC speed
.byte 0, 0, 0, 0, 0, 0, 0, 0				; Bank values
.word $4E20									; PAL speed
.byte 0   									; Flags, NTSC
.byte EXPANSION_FLAG
.byte 0, 0, 0, 0							; Reserved

;.include "driver.s"
