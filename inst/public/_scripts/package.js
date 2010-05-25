// set the rating performed by ajax
function get_rating(pkg_name)
{
  jQuery.ajax({
    url: "/packages/"+pkg_name+"/rating.json",
    dataType: "json",
    success: function(inner_html) {
      document.getElementById("rating").innerHTML = inner_html;
    }
  })
  
}

