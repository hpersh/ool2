
SUBDIRS	= $(shell find . -mindepth 1 -maxdepth 1 -type d)

clean:
	rm -f *~ *#
	for d in $(SUBDIRS); do make -C $$d $@; done