function LoadDynamicFeedControl() {
  var feeds = [
  {
    title: 'Inside-R',
    url: 'http://inside-r.org/blogs/feed'
  }];
  var options = {
    stacked : false,
    horizontal : true,
    title : ""
  }

  new GFdynamicFeedControl(feeds, 'feed-control', options);
}
// Load the feeds API and set the onload callback.
google.load('feeds', '1');
google.setOnLoadCallback(LoadDynamicFeedControl);
