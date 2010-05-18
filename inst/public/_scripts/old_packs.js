function hasClass(ele,cls) {
	return ele.className.match(new RegExp('(\\s|^)'+cls+'(\\s|$)'));
}

function addClass(ele,cls) {
	if (!this.hasClass(ele,cls)) 
	 ele.className += " "+cls;
}

function removeClass(ele,cls) {
	if (hasClass(ele,cls)) {
    	var reg = new RegExp('(\\s|^)'+cls+'(\\s|$)');
		ele.className=ele.className.replace(reg,' ');
	}
}


var library_update_count = 0;
var visible_update_count = 0;
var has_ran_old_packs = 0;

function highlight_old_packages()
{
  // start spinning
  var out_of_date_butto = document.getElementById("out_of_date_button");
  out_of_date_butto.value = "Thinking...";
  out_of_date_butto.disabled = true;
  
  
  jQuery.ajaxSync({
  	url: "/packages/old.json",
  	success: function(html)
  	{  
  	  var packs = JSON.parse(html);
  	  var i;
      library_update_count = 0;
      visible_update_count = 0;
      has_ran_old_packs = 1;
  	  
      // change the class name according to the package status
  	  for(i = 0; i < packs.length; i++){
  	    var row = document.getElementById("" + packs[i].Package);

  	    if(row != null){
  	      var pack_status = packs[i].status;
  	      
  	      addClass(row, pack_status)
  	      
  	      if(pack_status == "out_of_date"){
      	    library_update_count++;
      	    
      	    if(!hasClass(row, "hide"))
      	     visible_update_count++; 
  	      }
      	     
  	    }
  	  }
  	  
  	  var out_of_date_butto = document.getElementById("out_of_date_button");

  	  if(visible_update_count > 0){
    	  out_of_date_butto.value = "Update " + need_to_update + " "+pluralize(need_to_update, "Package");
    	  out_of_date_butto.onclick = update_packs;
    	  out_of_date_butto.disabled = false;
  	  }
  	  else{
    	  out_of_date_butto.value = "All up to date";
    	  out_of_date_butto.onClick = "";
    	  //out_of_date_butto.disabled = true;
  	  }
  	  
  	  
  	  // stop spinning
  	}
  });
}

function update_packs()
{
  var out_of_date_butto = document.getElementById("out_of_date_button");
  out_of_date_butto.value = "Updating...";
  out_of_date_butto.disabled = true;

   jQuery.ajaxSync({
  	url: "/packages/update.json",
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

showing_all_packages = 1;

function show_all_packages(){
  
  jQuery.ajaxSync({
  	url: "/packages/index.json",
  	success: function(html)
  	{
  	 var i;
  	 window.console.log(html);
	   var packs = JSON.parse(html);

  	 for(i = 0; i < packs.length; i++){
        var row = document.getElementById("" + packs[i].Package);
        
        if(row != null){
          
          var loaded = packs[i].isLoaded;
          window.console.log(packs[i]);
          if(showing_all_packages == 0){
            if(!loaded){
              addClass(row, 'hide')
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

      showing_all_packages = (showing_all_packages+1) % 2;
  
  	
  	}
  })  
}



