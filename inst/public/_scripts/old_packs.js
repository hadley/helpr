// determines if the element has a class or not
function hasClass(ele,cls) {
	return ele.className.match(new RegExp('(\\s|^)'+cls+'(\\s|$)'));
}

// adds a class to the element, if it doesn't exist already
function addClass(ele,cls) {
	if (!this.hasClass(ele,cls)) 
	 ele.className += " "+cls;
}

// removes a class from an element if it exists
function removeClass(ele,cls) {
	if (hasClass(ele,cls)) {
    	var reg = new RegExp('(\\s|^)'+cls+'(\\s|$)');
		ele.className=ele.className.replace(reg,' ');
	}
}


// highlight all old, out-of-date packages
var has_exec_highlight = 0;
function highlight_old_packages()
{
  // start spinning
  var out_of_date_butto = document.getElementById("out_of_date_button");
  out_of_date_butto.value = "Thinking...";
  out_of_date_butto.disabled = true;
  has_exec_highlight = 1;  
  
  jQuery.ajaxSync({
  	url: "/ajax/old_packages.html",
  	success: function(html)
  	{  
  	  var packs = JSON.parse(html);
  	  var i;
      
      // change the class name according to the package status
  	  for(i = 0; i < packs.length; i++){
  	    var row = document.getElementById("" + packs[i].Package);

        // if the row exists, add the class (status) to the row
  	    if(row != null){
  	      var pack_status = packs[i].status;
  	      set_status_to(row, pack_status);
  	    }
  	  }
  	  
  	  set_update_button();
  	  
  	  // stop spinning
  	}
  });
}

// make sure other classes are removed before adding new class
function set_status_to(element, pack_status){
  
  if(pack_status == "out_of_date"){
    removeClass(element, "updated");
  }else if(pack_status == "updated"){
    removeClass(element, "out_of_date");    
  }
  
  addClass(element, pack_status);
}


// update all out-of-date packages
function update_packs()
{
  var out_of_date_butto = document.getElementById("out_of_date_button");
  out_of_date_butto.value = "Updating...";
  out_of_date_butto.disabled = true;

   jQuery.ajaxSync({
  	url: "/ajax/update_packs.html",
  	success: function(html)
  	{
      highlight_old_packages();
  	}
  });
 
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

var showing_all_packages = 1;
function show_all_packages(){
  
  jQuery.ajaxSync({
  	url: "/ajax/package_list.html",
  	success: function(html)
  	{
  	 var i;
	   var packs = JSON.parse(html);

  	 for(i = 0; i < packs.length; i++){
        var row = document.getElementById("" + packs[i].Package);
        
        if(row != null){
          
          var loaded = packs[i].isLoaded;
          if(showing_all_packages == 0){
            if(!loaded){
              set_status_to(row, 'hide')
            }
          }else{
            if(!loaded){
              removeClass(row, 'hide')
            }
          }
             
        }  	   
  	 }
  	 
  	   	 
    	if(showing_all_packages == 0){
        document.getElementById("show_all_packs_button").value = "Show All Packages";
    	}else{
        document.getElementById("show_all_packs_button").value = "Show Loaded Packages";
    	}
    	
    	set_update_button();

      showing_all_packages = (showing_all_packages+1) % 2;
  
  	
  	}
  })  
}



