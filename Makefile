
all:
	@pandoc --pdf-engine=lualatex -V mainfont="DejaVu Sans" docs/README.md -o docs/README.pdf
	@sbt stage
	@sbt universal:packageBin
	@mv target/universal/*.zip .

.PHONY: clean
clean:
	@rm *.zip

