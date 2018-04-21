'''
Author: Michael Gryncewicz

Python code used for extracting and subsetting /r/technology posts from full data json.
'''

def extractSubredditData(subreddit_name, original_data_path):
    import json
    import pandas as pd

    count = 0
    count_tots = 0
    posts = pd.DataFrame()
    with open('reddit_{}.csv'.format(subreddit_name), 'w') as empty_file:
        pass

    with open(original_data_path) as f:
        for line in f:
            if count != 0:
                post = json.loads(line)
                if post['subreddit'] == subreddit_name:
                    posts = posts.append(post, ignore_index = True)
                    count += 1
                    if count%10000 == 0:
                        with open('reddit_{}.csv'.format(subreddit_name),'r') as infile:
                            posts.to_csv('reddit_{}.csv'.format(subreddit_name), mode = 'a', header = False)
                        posts = pd.DataFrame()
                count_tots += 1
                if count_tots%1000000 == 0:
                     print('{} posts finished...'.format(count_tots))
            else: # if first line, add header to file
                post = json.loads(line)
                if post['subreddit'] == subreddit_name:
                    posts = posts.append(post, ignore_index = True)
                    with open('reddit_{}.csv'.format(subreddit_name),'r') as infile:
                        posts.to_csv('reddit_{}.csv'.format(subreddit_name), mode = 'a')
                    posts = pd.DataFrame()
                    count += 1
                    print('Added first {} line with header'.format(subreddit_name))
                count_tots += 1

        posts.to_csv('reddit_{}.csv'.format(subreddit_name), mode = 'a', header = False)
    print('Finished file - {} posts found in {} subreddit'.format(count, subreddit_name))


extractSubredditData('technology', '/Users/MikeGryncewicz/Downloads/RS_2018-02')
