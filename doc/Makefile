build: example1.posm  example2.posm

example1.posm: posm-spec.txt
	awk '/__EXAMPLE1_START__/{flag=1;next}/__EXAMPLE1_END__/{flag=0}flag' posm-spec.txt | grep -v '^[[:space:]]*;;' > example1.posm

example2.posm: posm-spec.txt
	awk '/__EXAMPLE2_START__/{flag=1;next}/__EXAMPLE2_END__/{flag=0}flag' posm-spec.txt | grep -v '^[[:space:]]*;;' > example2.posm

clean:
	rm -f example1.posm  example2.posm
