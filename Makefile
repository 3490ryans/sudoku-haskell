build:
	stack setup
	stack install

run:
	bin/sudoku-exe

clean:
	rm bin/sudoku-exe
	stack clean