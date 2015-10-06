#!/usr/bin/perl

while(<>){
	push(@arr,$_);
}
$stra = join("",@arr);

$stra =~ s/\n/ /g;

#drop comment
$stra =~ s/\(\*.+?\*\)//g;

#insert LF
$stra =~ s/Cell\[/\nCell\[/g;
@arr = split(/\n/,$stra);

print "Notebook[{\n";
foreach(@arr){
	if($_ =~ /, \"Input\", /){
		print "$_\n";
	}
}
print "}]";
