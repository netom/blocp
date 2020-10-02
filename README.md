# blocp - copy blocks of data

BloCP is useful for making backups of large files, where there might
be little change between backups, and one does not want to write too
much data.

An example would be to periodically save virtual machine images or
disk partitions to SSDs. Transferring the files fully again and again
might fatigue the  SSD quickly.

BloCP reads and compares data, and only writes when necessary,
avoiding unnecessary writes.
