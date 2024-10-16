find . -type d -path '*/xyz/stratalab/*' | while read dir; do
  new_dir=$(echo $dir | sed 's|xyz/stratalab|org/plasmalabs|g')
  new_dir=$(echo $new_dir | sed 's|strata|plasma|g')

  mkdir -p $new_dir
  echo "Renaming $dir to $new_dir"
  git mv $dir $new_dir -f
  git rm -r $dir
  git add $new_dir
done

git clean -xdf
