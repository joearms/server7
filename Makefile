all:
	cd src; make
	cd site; make
	erl -pa src -pa site -pa tmp -pa tmp/cowboy/src -pa tmp/cowlib/src\
            -pa tmp/ranch/src -s starter start
