# M3 Editor
Program to directly edit M3 models

# Executable files for Windows
* Download latest version from [Release](https://github.com/tangorcraft/m3editor/releases) page.
* [Download M3 Editor v0.8 executable files for Windows](https://github.com/tangorcraft/m3editor/releases/download/v0.8/m3editor-win.zip)

## Tree View Editor
* Selecting tag on the left side will display fields of 1st element of that tag in the table.
  * To edit a field doubleclick on field row you want to edit.
  * If selected tag has more than one item, to change index of item to edit use `<<` and `>>` buttons.
  * If selected tag item has references to other tags in it's fields, these tags will be displayed as sub-nodes of selected tag.
  * If selected tag is referenced from other tags, those references will be displayed at the bottom of values table.
    * Doubleclicking on "Referenced from" row will select a tag in tag tree and item index that references current tag.
* Tag at index 0 (`MD33` or `MD34`) is special as it's fields values describe **.m3** file offsets.
  * Any changes will be rewriten when file is saved.

## CHAR bulk edit
* Allows to edit all CHAR tags' values in a single action.
* Text can be copied to any text editor and then copied back to apply changes on tags.
* Lines that don't need to be changed can be deleted.
* Number at the start of the line and colon is an index of CHAR tag and must be preserved for changed lines.

## Texture rename tool
* Call it from menu: Model - Rename textures.
* Allows to rename textures in a more user-friendly way.
* Also allows to change `colorChannelSetting` of a LAYR tag(s), that use this texture path.
* If there is more than one LAYR tag referencing same texture path with different `colorChannelSetting` it will be shown as `Various`.
  * Selecting `Various` in `colorChannelSetting` edit means no changes will be made. Any other value will overwrite `colorChannelSetting` for all LAYR tags that use this texture.

## M3ML Export
* It is an xml file, that preserves tags structure.
* Simply open **.m3ml** file, there is no import/merge feature for now.
* Some information exported to **.m3ml** file is not used by parser and is added to make editing simpler. This includes:
  * field names.
  * `refFrom` tags.
* References will be scanned after import and `refCnt` will be corrected to match the item count of referenced tag.
  * `refCnt=0` **or** `refIdx=0` means "No reference".
* `m3tag` tags without `Idx` attribute will be added to the end of tag list.

# License (GPL 3.0 or later)
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
