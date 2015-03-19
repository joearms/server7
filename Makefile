all:
	cd src; make
	erl -pa src -pa site -pa tmp/cowboy/src -pa tmp/cowlib/src\
            -pa tmp/ranch/src -s starter start
