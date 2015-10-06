#!/usr/bin/perl

while(<>){
	push(@arr,$_);
}
$stra = join("",@arr);

$stra =~ s/\s/ /g;
$stra =~ s/\\ / /g;
$stra =~ s/ +/ /g;

#drop comment
$stra =~ s/\(\*.+?\*\)//g;

#insert LF
$stra =~ s/Cell\[/\nCell\[/g;
@arr = split(/\n/,$stra);

print "Notebook[{\n";
foreach(@arr){
	if($_ =~ /BoxData.+, \"Input\", /){
		$_ =~ s/(^.+Input.+?\])(.*)/$1/;
		print "$_ ,\n";
	}
}
print "}]";
