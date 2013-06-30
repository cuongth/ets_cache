LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.2
PKGNAME=erlmc

all: emake

emake:
	erl -make
		
clean:
	rm -rf erl_crash.dump ebin/*.beam

install:
	mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	for i in ebin/*.beam ebin/*.app include/*.hrl; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
