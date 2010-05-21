function show_all_packages() {
  $("#packages").toggleClass("loaded");
}

// highlight all old, out-of-date packages
function highlight_old_packages() {
  button = $("#out_of_date_button");
  button.attr("value", "Thinking...");
  button.attr("disabled", true)

  jQuery.ajax({
    url: "/packages/old.json",
    dataType: "json",
    success: function(packages) {
      for(i = 0; i < packages.length; i++) {
        pkg = packages[i];
        $("#" + pkg).addClass("old");
      }
    }
  })
  button.attr("value", "Updated");
  
}

// update all out-of-date packages
function update_packs() {
  var out_of_date_butto = document.getElementById("out_of_date_button");
  out_of_date_butto.value = "Updating...";
  out_of_date_butto.disabled = true;

  jQuery.ajax({
    url: "/packages/update.json",
    dataType: "json",
    success: function(packages) {
      for(i = 0; i < packages.length; i++) {
        pkg = packages[i];
        $("#" + pkg).removeClass("old").addClass("update");
      }
    }
  })
}

function pluralize(count, word)
{
  if(count > 1)
    return word + "s";
  else
    return word;
}

function set_update_button(){

  // if highlight_old_packages has not been run, return 
  if(has_exec_highlight == 0)
    return;

  var i;
  var count = 0;
  
  // count all visible rows that are out_of_date
  var packs = document.getElementById("packages").tBodies[0];
  for(i = 0; i < packs.childNodes.length; i++){
    if(packs.childNodes[i].nodeType==1 && hasClass(packs.childNodes[i], "out_of_date") && !hasClass(packs.childNodes[i], "hide")){
      count++;
    }
  }
  
  // set state of update button
  var out_of_date_butto = document.getElementById("out_of_date_button");
  if(count > 0){
    out_of_date_butto.value = "Update " + count + " "+pluralize(count, "Package");
    out_of_date_butto.onclick = update_packs;
    out_of_date_butto.disabled = false;
  }
  else{
    out_of_date_butto.value = "All up to date";
    out_of_date_butto.onClick = "";
    out_of_date_butto.disabled = true;
  }

}

