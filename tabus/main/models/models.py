"""
"""

from __future__ import unicode_literals

from django.db import models



class User(models.Model):
    username = models.CharField(max_length = 20, unique=True)
    def __str__(self):
        return str(self.username)

class Sentence(models.Model):
    adding_date = models.DateTimeField()
    owner = models.ForeignKey(User, on_delete=models.CASCADE, db_index = True)
    content = models.CharField(max_length = 250)
    def __str__(self):
        return str(self.content)

    class Meta:
        unique_together = (('owner', 'content'),)

class Tag(models.Model):
    tag = models.CharField(max_length = 20, unique=True)
    owner = models.ForeignKey(User, on_delete=models.CASCADE, db_index = True)
    def __str__(self):
        return str(self.tag)

    class Meta:
        unique_together = (('tag', 'owner',),)

class TagSentence(models.Model):
    tag = models.ForeignKey(Tag, on_delete=models.CASCADE, db_index = True)
    sentence = (
        models.ForeignKey(Sentence, 
            on_delete=models.CASCADE, db_index = True))

    class Meta:
        unique_together = (('tag', 'sentence',),)