install: build placeFiles

udate: updateSrc install

placeFiles:
	sudo cp hsh /bin
	sudo mkdir -p /usr/share/hsh
	sudo cp preluderc /usr/share/hsh

build:
	ghc -O2 -dynamic -Wall -j8 hsh

clean:
	rm *.hi *.o *.hi-boot *.o-boot

udateSrc:
	git pull
