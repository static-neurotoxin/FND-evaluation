# Data formats

We exchange data, often in widely defined formats. Unfortunately, **NOT INVENTED HERE** occurs far too frequently, so there is a proliferation of data exchange formats that largely do the same thing. Standards are awesome, pick one you like!

## Code blocks

### CSS

```css
--numerals: 1234567890;
--similar: "oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_=";
--diacritics-etc: "â é ù ï ø ç Ã Ē Æ œ";

button {
    all: unset;
    display: flex;
    width: 20px;
    margin: 0 .5ex 0 0;
    border: 1px solid var(--light-grey);
    border-radius: 3px;
    background: linear-gradient(var(--bright-white), var(--light-grey));
    color: var(--medium-grey);
    cursor: pointer;

    /* handle dark-mode */
    @media (prefers-color-scheme: dark) {
        background: linear-gradient(var(--light-grey), var(--bright-white));
    }

    path,
    rect {
        fill: currentColor;
    }

    &:hover {
        color: var(--ink-black);
    }
    &.selected {
        background: var(--bright-white);
        color: var(--ink-black);
        box-shadow: none;
    }
    &:not(.selected) > svg:not(:first-child),
    &.selected > svg:not(.selected) {
        display: none;
    }

    &:disabled {
        color: var(--paper-white);
        background: var(--light-grey);
    }

    &.text-button {
        width: auto;
        margin: 0;
        padding: 0 .5ex;
    }
}
```

### ASCII Art

```text
1. Arduino Pinout ASCII Art:
   from https://github.com/busyDuckman/ascii-art-arduinos

								  +-----+
	 +----[PWR]-------------------| USB |--+
	 |                            +-----+  |
	 |         GND/RST2  [ ][ ]            |
	 |       MOSI2/SCK2  [ ][ ]  A5/SCL[ ] |   C5
	 |          5V/MISO2 [ ][ ]  A4/SDA[ ] |   C4
	 |                             AREF[ ] |
	 |                              GND[ ] |
	 | [ ]N/C                    SCK/13[ ] |   B5
	 | [ ]IOREF                 MISO/12[ ] |   .
	 | [ ]RST                   MOSI/11[ ]~|   .
	 | [ ]3V3    +---+               10[ ]~|   .
	 | [ ]5v     | A |                9[ ]~|   .
	 | [ ]GND   -| R |-               8[ ] |   B0
	 | [ ]GND   -| D |-                    |
	 | [ ]Vin   -| U |-               7[ ] |   D7
	 |          -| I |-               6[ ]~|   .
	 | [ ]A0    -| N |-               5[ ]~|   .
	 | [ ]A1    -| O |-               4[ ] |   .
	 | [ ]A2     +---+           INT1/3[ ]~|   .
	 | [ ]A3                     INT0/2[ ] |   .
	 | [ ]A4/SDA  RST SCK MISO     TX>1[ ] |   .
	 | [ ]A5/SCL  [ ] [ ] [ ]      RX<0[ ] |   D0
	 |            [ ] [ ] [ ]              |
	 |  UNO_R3    GND MOSI 5V  ____________/
	  _______________________/


2. Box Drawing Characters:
   from https://en.wikipedia.org/wiki/Box-drawing_characters

	 ┌───────────────────┐
	 │  ╔═══╗ Some Text  │▒
	 │  ╚═╦═╝ in the box │▒
	 ╞═╤══╩══╤═══════════╡▒
	 │ ├──┬──┤           │▒
	 │ └──┴──┘           │▒
	 └───────────────────┘▒
	  ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒


3. UUIDv6:
   from https://planetscale.com/blog/the-problem-with-using-a-uuid-primary-key-in-mysql#uuidv6

			   ╭─── time_mid       ╭── node
			  ╭┶─╮           ╭─────┶────╮
	 e54af2ec-d381-11ee-a506-0242ac120002
	 ╰──┮───╯      ╰─┮╯ ╰─┮╯
	 time_hi         │    ╰── clock_sequence_and_version
					 ╰─────── time_low_and_version
```

### XML

```xml
<?xml version="1.0" encoding="UTF-8"?>
as well as operators.
-->
<system-config version="2.0">
	<user-prefs>
		<setting key="font_size" value="14pt" />
		<setting key="auto_save_enabled" value="true" />
	</user-prefs>

	<data-source name="main_db">
		<query>
			<![CDATA[
			  SELECT user_id, user_name, email
			  FROM users
			  WHERE age >= 18
				AND status != 'pending';
			]]>
		</query>
	</data-source>

</system-config>
```

### YAML

```yaml
# Application configuration settings
server:
  host: 127.0.0.1
  port: 8080
  security:
	enabled: true
	tls_version: 1.3

database:
  user: "admin"
  # Use environment variables for passwords in production
  password: "${DB_PASSWORD}"
  pool_size: 20
  # List of replica nodes
  replicas:
	- host: "replica1.db.internal"
	  port: 5432
	- host: "replica2.db.internal"
	  port: 5432
```

### TOML

```toml
# Main project metadata
title = "TOML Example Project"
version = "1.0.0"
authors = ["Example User <user@example.com>"]

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

# Defines an array of tables for different users
[[users]]
name = "Alice"
roles = [ "admin", "editor" ]

[[users]]
name = "Bob"
roles = [ "viewer" ]
```

### JSON

```json
{
  "userId": "a0a1b2c3-d4e5-f6a7-b8c9-d0e1f2a3b4c5",
  "username": "jdoe",
  "email": "johndoe@example.com",
  "isActive": true,
  "lastLogin": "2025-07-28T14:10:00Z",
  "profile": {
	"fullName": "John Doe",
	"avatarUrl": "https://example.com/avatars/jdoe.png"
  },
  "roles": [
	"contributor",
	"editor"
  ],
  "managerId": null
}
```

### INI

```ini
[display]
encoding = "Unicode"
version = 16

[test]
enable_bugs = 1
frustration_level = "Maximum++"
performance_level = 1979

```

### CSV

```csv
key,value
foo,1
bar,2
baz,3
```

### TSV

```tsv
key	value
foo	1
bar	2
baz	3
```

### Motorola S-Record

```srec
S00F000068656C6C6F202020202000003C
S11F00007C0802A6900100049421FFF07C6C1B787C8C23783C6000003863000026
S11F001C4BFFFFE5398000007D83637880010014382100107C0803A64E800020E9
S111003848656C6C6F20776F726C642E0A0042
S5030003F9
S9030000FC
```

### Intel Hex

```ihex
:10010000214601360121470136007EFE09D2190140
:100110002146017E17C20001FF5F16002148011928
:10012000194E79234623965778239EDA3F01B2CAA7
:100130003F0156702B5E712B722B732146013421C7
:00000001FF
```

### HexDump

``` hexdump
00000000  23 20 42 6c 75 65 20 4f  61 6b 20 4d 6f 64 65 6c  |# Blue Oak Model|
00000010  20 4c 69 63 65 6e 73 65  0a 0a 56 65 72 73 69 6f  | License..Versio|
00000020  6e 20 31 2e 30 2e 30 0a  0a 23 23 20 50 75 72 70  |n 1.0.0..## Purp|
00000030  6f 73 65 0a 0a 54 68 69  73 20 6c 69 63 65 6e 73  |ose..This licens|
00000040  65 20 67 69 76 65 73 20  65 76 65 72 79 6f 6e 65  |e gives everyone|
00000050  20 61 73 20 6d 75 63 68  20 70 65 72 6d 69 73 73  | as much permiss|
00000060  69 6f 6e 20 74 6f 20 77  6f 72 6b 20 77 69 74 68  |ion to work with|
00000070  0a 74 68 69 73 20 73 6f  66 74 77 61 72 65 20 61  |.this software a|
00000080  73 20 70 6f 73 73 69 62  6c 65 2c 20 77 68 69 6c  |s possible, whil|
00000090  65 20 70 72 6f 74 65 63  74 69 6e 67 20 63 6f 6e  |e protecting con|
000000a0  74 72 69 62 75 74 6f 72  73 0a 66 72 6f 6d 20 6c  |tributors.from l|
```
