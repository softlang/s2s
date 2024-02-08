
all:
	@pandoc --pdf-engine=lualatex -V mainfont="DejaVu Sans" docs/README.md -o docs/README.pdf
	@sbt stage
	@sbt universal:packageBin
	@cp target/universal/*.zip "s2s.zip"

.PHONY: clean
clean:
	@rm *.zip

