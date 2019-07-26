install: 
	ghc -O2 -dynamic hsh -Wall
	sudo cp hsh /bin

clean:
	rm *.hi *.o *.hi-boot *.o-boot
