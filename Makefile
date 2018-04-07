TARGET=cybergrid
.PHONE: all clean

all: $(TARGET).prg

run: $(TARGET).prg
	x64 -joydev2 1 $(TARGET).prg || true

$(TARGET).prg: $(TARGET).asm titlescreen.prg cyber.prg
	dasm $(TARGET).asm -o$(TARGET).prg

clean:
	rm -f $(TARGET).prg
