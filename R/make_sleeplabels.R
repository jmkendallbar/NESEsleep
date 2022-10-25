# Function to turn current labels into desired labels
make_waterlabels <- function(orig_labels) {
  label_dict <- list(
    `Instrument ON Animal` = "LAND",
    `Animal Exits Water` = "LAND",
    `Animal Enters Water` = "SHALLOW WATER",
    `Animal Returns to Shallow Water` = "SHALLOW WATER",
    `Animal Leaves Shallow Water` = "DEEP WATER",
    `Animal Returns to Continental Shelf Water` = "DEEP WATER",
    `Animal Leaves Continental Shelf Water` = "OPEN OCEAN"
  )
  if (any(!orig_labels %in% names(label_dict))){
    warning("One or more labels are not in the label dictionary.")
  }
  as.character(label_dict[orig_labels])
}
