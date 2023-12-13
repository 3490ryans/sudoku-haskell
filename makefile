build:	
	stack setup
	stack install

run:
	cd bin && ./sudoku-exe

clean:
	rm bin/sudoku-exe
	stack clean
