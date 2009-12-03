(def-om NSInteger numberOfSectionsInTableView
  [UITableView* tableView]
  (:count (.results self)))