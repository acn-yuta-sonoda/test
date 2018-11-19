data testdata2;
	infile '/folders/myshortcuts/SAS/testdata2.csv' dsd;
	input id japanese math english science sociology sum;

proc princomp data=testdata2;
	var japanese math english science sociology;

proc factor data=testdata2 mineigen=0 method=principal simple corr;
	var japanese math english science sociology;
run;
