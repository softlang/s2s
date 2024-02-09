all:
	@pandoc --pdf-engine=lualatex -V mainfont="DejaVu Sans" docs/README.md -o docs/README.pdf
	@sbt stage
	@sbt Universal/packageBin
	@mv target/universal/*.zip .

.PHONY: clean
clean:
	@sbt clean
	@rm -f s2s.zip
	@rm -f docs/README.pdf

