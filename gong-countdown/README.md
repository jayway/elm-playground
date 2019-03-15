# Gong countdown

This is a countdown to the gong in the Malmö office. It will fire off an http request at a predetermined time to automatically hit the gong. 10 minutes before that, it will display a timer on the signage system in the office.

Anyone at Jayway is welcome to contribute to this project!


## How to use

The gong can be configured using query params.

### Message

`?message=Breakfast at the 5th floor!`

### Time

`?hour=16&minute=00`

### Full example

`<Base path>?message=Breakfast at the 5th floor!&hour=16&minute=00`
