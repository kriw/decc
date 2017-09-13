fileName=$1
tmp='./.objdump.out'
objdump -d -Mintel $fileName > $tmp
./bin/decc $tmp
rm $tmp
