# mp-lookup

A toy Haskell application for batch retrieving information about members of parliament for UK postcodes. This application started off as a simple way to gain familiarity with core Haskell packages like [req](https://hackage.haskell.org/package/req), [aeson](https://hackage.haskell.org/package/aeson), and [cassava](https://hackage.haskell.org/package/cassava). Adding a TUI built with [brick](https://hackage.haskell.org/package/brick) introduced a number of additional concepts like lenses and concurrency.

The information about MPs is retrieved using the [UK Parliament Members API](https://members-api.parliament.uk/index.html).

### Running the application
Stack has been used from the beginning to build and run the application. `stack run` should take care of downloading dependencies, building and launching the TUI.

### Future changes
I've outlined the direction I want to take the application in the ever-growing [TODO](./TODO.md) file; there are no guarantees that I'll ever finish these before getting distracted with another side project :)
