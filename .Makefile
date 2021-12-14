all:
	ghc -O2 main.hs
timeAnalysis: clean
	ghc -O2 --make main.hs -prof -fprof-auto && time ./main +RTS -p && notify-send "Time Analysis" "Complete" && nvim main.prof
clean:
	rm -f *.hi *.o main *.prof
