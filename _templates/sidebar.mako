<div id="right_sidebar">
  <div id="links">
  <h3>About Tim</h3>
  <ul>
      <li><img src="/site_img/icon-github.png" width="16" height="16" alt="Github" valign="middle"/> Github - <a href="https://github.com/timrobinson/">timrobinson</a></li> 
      <li><img src="/site_img/icon-twitter.png" width="16" height="16" alt="Twitter" valign="middle"/> Twitter - <a href="http://twitter.com/1tgr">@1tgr</a></li> 
  </ul>
  </div>
  <div id="blog_post_list">
  <h3>Latest blog posts</h3>
  <ul>
% for post in bf.config.blog.posts[:5]:
    <li><a href="${post.path}">${post.title}</a></li>
% endfor
  </ul>
  </div>
  <div id="categories">
    <h3>Categories</h3>
    <ul>
% for category, num_posts in bf.config.blog.all_categories:
     <li><a href="${category.path}">${category}</a> (<a href="${category.path}/feed">rss</a>) (${num_posts})</li>
% endfor
    </ul>
  </div> 
  <div id="archives">			
    <h3>Archives</h3>
    <ul>
% for link, name, num_posts in bf.config.blog.archive_links:
      <li><a href="${bf.util.site_path_helper(bf.config.blog.path,link)}/1" title="${name}">${name}</a>&nbsp;(${num_posts})</li>
% endfor
    </ul>
  </div>

</div>
