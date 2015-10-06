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
	$line = "";
	if($_ =~ /^Cell/){
		$line = $_;
	}
	if($line =~ /CellChangeTimes.+CellChangeTimes/){
		$line =~ s/(CellChangeTimes.+?\}\])/$1 \n/g;
		@tmparr = split(/\n/,$line);
		$line = shift(@tmparr);
	}
	if($line =~ /, \"[a-zA-Z]*[Ss]ection\", /){
		$line =~ s/(Cell.+ \"[a-zA-Z]*[Ss]ection.+?\])(.*)/$1/;
		print "$line ,\n";
	}
	elsif($line =~ /BoxData.+, \"Input\", /){
		$line =~ s/(^.+Input.+?\])(.*)/$1/;
		print "$line ,\n";
	}
}
print "}]\n";
