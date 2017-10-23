#this script copy and organize the files for the resting state analyis of multipams
#it assumes that the relevant folder on the nas has been mounted on a local folder called k:

$text = Get-Content ("D:\MultiPAMS\sujets_MultiPAMSCtrl.txt")
$dir_root = "k:\"
$target_dir = "D:\MultiPAMS\iron\Image"

For ($index=0; $index -lt $text.Length; $index++) {
	$subject = $text[$index]
	Copy-Item -Path ($dir_root + $subject + "\*nii.gz") -Destination $target_dir
	
}