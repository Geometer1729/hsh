install: 
	#Types compiled early so Parse can be compiled with different warnings
	ghc -O2 -dynamic Types -Wall
	#Parse compiled with unused-do-bind warnings disabled because string is :: ReadP String
	ghc -O2 -dynamic Parse -Wall -Wno-unused-do-bind
	ghc -O2 -dynamic hsh -Wall 
	sudo cp hsh /bin

clean:
	rm *.hi *.o *.hi-boot *.o-boot
