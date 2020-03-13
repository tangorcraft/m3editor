# M3 Editor
Program to directly edit M3 models

## Tree View Editor
* Selecting tag on the left side will display fields of 1st element of that tag in the table.
  * To edit a field doubleclick on value column of the field you want to edit.
  * If selected tag has more than one item, to change index of item to edit use `<<` and `>>` buttons.
  * If selected tag item has references to other tags in it's fields, these tags will be displayed as sub-nodes of selected tag.
  * If selected tag is referenced from other tags, those references will be displayed at the bottom of values table.
    * Doubleclicking on value of "Referenced from" field will select a tag in tag tree and item index that references current tag.
* Adding/deleting tags and tag items is not implemented yet.
* Tag at index 0 (`MD33` or `MD34`) is special as it's fields values describe **.m3** file offsets.
  * Any changes will be rewriten when file is saved.

## CHAR bulk edit
* Allows to edit all CHAR tags' values in a single action.
* Text can be copied to any text editor and then copied back to perform changes on tags.
* Lines that don't need to be changed can be deleted.
* Number at the start of the line and colon is an index of CHAR tag and must be preserved for changed lines.

## M3ML Export
* It is an xml file, that preserves tags structure.
* Simply open **.m3ml** file, there is no import/merge feature for now.
* Some information exported to **.m3ml** file is not used by parser and is added to make editing simpler. This includes:
  * field names.
  * `refFrom` tags.
* References will be scanned after import and `refCnt` will be corrected to match the item count of referenced tag.
  * `refCnt=0` or `refIdx=0` means "No reference".
* `m3tag` tags without `Idx` attribute will be added to the end of tag list.
