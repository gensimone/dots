eval "$(awk 'NR==1{print "wallpaper=\"" $0 "\""} NR==2{print "fastfetch_image=\"" $0 "\""} NR==3{print "color_name=\"" $0 "\""}' "$1")"

echo $wallpaper
echo $fastfetch_image
echo $color_name
