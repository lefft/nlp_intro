
# coding: utf-8

# ## Spam Classification Demo <hr>

# ### 1. Data Acquisition

# In[1]:


import pandas as pd

# read in data and reorganize a bit 
spam_data = '../sample_datasets/spam_clf/uci_ml_spam.txt'
dat = pd.read_csv(spam_data, names=['ham_spam', 'text'], sep='\t')

dat['label'] = dat.ham_spam == 'spam'
dat = dat[['ham_spam', 'label', 'text']]


# In[2]:


# inspect the dataset 
print('spam data dimensions: {shape}'.format(shape=dat.shape))
print(dat.head(5))


# In[3]:


print('distribution of labels:')
dat.ham_spam.value_counts() # / sum(dat.ham_spam.value_counts())


# In[4]:


from random import sample

# test-train split 
train_idx = sample(range(len(dat)), int(len(dat)*.75))
test_idx = [n for n in set(range(len(dat))) - set(train_idx)]

train_text = list(dat.text.iloc[train_idx])
train_labs = list(dat.label.iloc[train_idx])

test_text = list(dat.text.iloc[test_idx])
test_labs = list(dat.label.iloc[test_idx])


# ### 2. Preprocessing

# In[5]:


from sklearn.feature_extraction.text import CountVectorizer

# preprocess text (cast to a count-based document-term matrix)
vectorizer = CountVectorizer(lowercase=True, ngram_range=(1,1), 
                             stop_words='english')
train_dtm = vectorizer.fit_transform(train_text)

# check out the transformed data 
# vectorizer.get_feature_names()[999:1011]
# list(vectorizer.get_stop_words())[0:10]
# vectorizer.get_params()
# pd.DataFrame(train_dtm.A, columns=vectorizer.get_feature_names()).to_string())


# ### Model Training

# In[6]:


from sklearn.naive_bayes import MultinomialNB

# model training 
clf = MultinomialNB()
clf.fit(train_dtm, train_labs)


# ### Model Evaluation

# In[7]:


# model evaluation 
test_dtm = vectorizer.transform(test_text)

predictions = clf.predict(test_dtm)


# In[8]:


from sklearn import metrics

accuracy = metrics.accuracy_score(test_labs, predictions)
print(f'out of the box accuracy: {round(accuracy,3)} (waow)')


# In[9]:


eval_df = pd.DataFrame([*zip(test_labs, predictions, test_text)], 
                       columns=('label', 'prediction', 'text'))

print(eval_df.head(5))


# In[10]:


# can also add `margins=True` 
conf_mat = pd.crosstab(eval_df.label, eval_df.prediction, 
                       rownames=['observed'], colnames=['predicted'])

print(conf_mat)


# In[11]:


# we can also view a confusion matrix with column-normalized values
print(round(conf_mat / conf_mat.sum(axis=0), 3))


# In[ ]:


# look at the bad predictions 
print(eval_df[eval_df.label != eval_df.prediction].sort_values('label'))

