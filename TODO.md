## APP TODO
- [x] Use newtypes instead of types  
- [x] Include political party  
- [ ] Create type for (Postcode, ResponseType)?  
- [x] Move Postcode into Lib
- [ ] Create parsing function that returns Maybe Postcode, remove Postcode ctor visibility
- [ ] Add header to CSV
- [x] Configure CSV package to serialize records 
- [ ] Use lenses for API response value retrieval 
- [ ] Move all CSV logic (including ToRecord instances) to separate module
- [ ] Create type/monad representing an effect that can can result in type a (success) or ErrorMessage (error case); essentially abstract type (Either ErrorMessag a)

## UI TODO
- [x] User can enter an input file to be read for postcodes
- [x] User is shown error when invalid file name is entered
- [x] User is shown results of postcode search in TUI
- [x] User is shown nicely formatted results in TUI
- [ ] User is given option to write results to file
- [ ] Hand off IO tasks (read file, make web requests) to background thread
- [ ] Progress bar is shown when background processing is happening
- [ ] User is presented with a file browser to select input file
- [ ] User is shown menu of different options (e.g. "Search for MP by postcode")
- [ ] User is given option to enter 1 or more postcodes manually or read them from a file
