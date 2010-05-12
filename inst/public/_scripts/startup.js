$(document).ready(function(){
  $('#packages_installed tr').quicksearch({
    position: 'before',
    attached: '#packages_installed',
    labelText: 'Search: ',
    fixWidths: true
  });
})
