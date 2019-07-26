install: build placeBin

placeBin:
	sudo cp hsh /bin

build:
	ghc -O2 -dynamic hsh -Wall 

clean:
	rm *.hi *.o *.hi-boot *.o-boot
