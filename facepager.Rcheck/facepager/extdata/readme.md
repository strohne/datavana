Example data to demonstrate how to use the package:
- The csv file contains example data from two fictional Facebook pages - posts and comments.
- The db file is a Facepager database with examples from Reddit.

## CSV file
A small dataset generated as an example, where the unit of analysis is either a post or a comment. The dataset is exactly in the same shape as you export it from Facepager as a csv file.
This means that the columns are named as they were requested from the API.

- level: node level, a value between 0 and 2
- id: own ID in the data set, chronological numbers starting at 1
- parent_id: ID of the parent node in the data set, from which the data was fetched
- object_id: own ID,in the given platform format, here two 15/16 digits numbers for the page and the post "XXX_XXX"
- object_type: kind of node, possible types: seed, data, offcut, unpacked
- object_key: keys for the further extraction in Facepager
- query_status: Success = fetched (200) or error codes of the request
- query_time: Time of the query via facepager
- query_type: which Facepager module and which resource were used for the query
- message: text of the post/comment
- created_time: time of publication 
- like_count: like count of the comments
- comment_count: comment count of the comments (replies)
- shares.count: shares count of the posts
- comments.summary.total_count: comment count of the posts
- permalink_url: URL of the posts
- reactions.summary.total_count: reaction count of the posts
- attachments.data.*.title: caption of the post, picture etc.
