files="undo.up.png undo.dn.png \
      new-game.up.png new-game.dn.png \
      restart.up.png restart.dn.png \
      help.up.png help.dn.png"
UP1=6ac75b
UP2=6b8522
DN1=b05bc7
DN2=622287
fm -f $files
wget -O undo.up.png "http://dabuttonfactory.com/b.png?t=Undo&f=Calibri-Bold&ts=23&tc=ffffff&tshs=1&tshc=222222&it=png&c=6&bgt=gradient&bgc=$UP1&ebgc=$UP2&hp=20&vp=7"
wget -O undo.dn.png "http://dabuttonfactory.com/b.png?t=Undo&f=Calibri-Bold&ts=23&tc=ffffff&tshs=1&tshc=222222&it=png&c=6&bgt=gradient&bgc=$DN1&ebgc=$DN2&hp=20&vp=7"
wget -O new-game.up.png "http://dabuttonfactory.com/b.png?t=New+game&f=Calibri-Bold&ts=23&tc=ffffff&tshs=1&tshc=222222&it=png&c=6&bgt=gradient&bgc=$UP1&ebgc=$UP2&hp=20&vp=7"
wget -O new-game.dn.png "http://dabuttonfactory.com/b.png?t=New+game&f=Calibri-Bold&ts=23&tc=ffffff&tshs=1&tshc=222222&it=png&c=6&bgt=gradient&bgc=$DN1&ebgc=$DN2&hp=20&vp=7"
wget -O restart.up.png "http://dabuttonfactory.com/b.png?t=Restart&f=Calibri-Bold&ts=23&tc=ffffff&tshs=1&tshc=222222&it=png&c=6&bgt=gradient&bgc=$UP1&ebgc=$UP2&hp=20&vp=7"
wget -O restart.dn.png "http://dabuttonfactory.com/b.png?t=Restart&f=Calibri-Bold&ts=23&tc=ffffff&tshs=1&tshc=222222&it=png&c=6&bgt=gradient&bgc=$DN1&ebgc=$DN2&hp=20&vp=7"
wget -O help.up.png "http://dabuttonfactory.com/b.png?t=Help&f=Calibri-Bold&ts=23&tc=ffffff&tshs=1&tshc=222222&it=png&c=6&bgt=gradient&bgc=$UP1&ebgc=$UP2&hp=20&vp=7"
wget -O help.dn.png "http://dabuttonfactory.com/b.png?t=Help&f=Calibri-Bold&ts=23&tc=ffffff&tshs=1&tshc=222222&it=png&c=6&bgt=gradient&bgc=$DN1&ebgc=$DN2&hp=20&vp=7"
for f in $files ; do
    identify $f
    convert $f -scale 128x128! resources/$f
done
rm -f $files
