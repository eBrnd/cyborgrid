TARGET=cybergrid
.PHONE: all clean

all: $(TARGET).prg

run: $(TARGET).prg
	x64 $(TARGET).prg || true

$(TARGET).prg: $(TARGET).asm titlescreen-bitmap.prg titlescreen-charmem.prg titlescreen-colormem.prg sprites.asm cyber.prg
	dasm $(TARGET).asm -o$(TARGET).prg

clean:
	rm -f $(TARGET).prg
