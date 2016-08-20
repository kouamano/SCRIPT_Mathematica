#!/usr/bin/perl

print "{";
while(<>){
	chomp;
	$_ =~ s/^\s+//ig;
	$_ =~ s/\s+$//ig;
	($h,$t) = split(/\t/,$_);
	if($h =~ /^[0-9]+$/){
		;
	}else{
		$h = "\"".$h."\"";
	}
	if($t =~ /^[0-9]+$/){
		;
	}else{
		$t = "\"".$t."\"";
	}
	print "{".$h.",".$t."}".",\n";
}
print "}\n";
