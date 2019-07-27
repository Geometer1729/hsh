install: build placeBin

udate: updateSrc clean install

placeBin:
	sudo cp hsh /bin

build:
	ghc -O2 -dynamic -Wall -j8 hsh

clean:
	rm *.hi *.o *.hi-boot *.o-boot

udateSrc:
	git pull
