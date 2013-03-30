SUBDIRS	= src module bld

clean:
	rm -f *~ *#
	for d in $(SUBDIRS); do make -C $$d $@; done