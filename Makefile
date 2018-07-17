default: all

SRCS := $(shell find dna -type f -name '*.ml')
BUNDLES := $(patsubst %.ml,%.bundle.js,$(SRCS))
BS := $(patsubst %.ml,%.bs.js,$(SRCS))


clean:
	yarn clean
	rm -f $(BUNDLES) $(BS)

dev:
	yarn build
bundle : $(BUNDLES)

all: dev
.PHONY:
	all dev bundle clean
