
all:
	@pandoc --pdf-engine=lualatex -V mainfont="DejaVu Sans" tutorial/README.md -o tutorial/README.pdf
	@sbt stage
	@sbt universal:packageBin
	@mv target/universal/*.zip .

.PHONY: clean
clean:
	@rm *.zip

