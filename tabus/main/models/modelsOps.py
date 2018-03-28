from django.db.models import Count
from django.utils import timezone

from main.models.models import *


def addEntry(user, sentence, tags):
    """
        @Description
            add to DB needed objects to keep all data
            from entry;
        @Args
            @user <- String
            @tags <- array of String
            @sentence <- String
        @Returns
            @boolean value indicating if action perform 
                with success
    """

    addUser(user)
    userDB = User.objects.get(username=user)
    tagDBs = []

    for tag in tags:
        addTag(tag, userDB)
        tagDB = Tag.objects.get(owner=userDB.id, tag=tag)
        tagDBs.append(tagDB)
    userDB.save()
    addSentence(sentence, userDB)

    sentenceDB = Sentence.objects.get(owner=userDB.id, content=sentence)

    for tagDB in tagDBs:
        tagSentence = TagSentence(tag=tagDB, sentence=sentenceDB)
        tagSentence.save()

    return True


def addUser(user):
    userDB = User.objects.filter(username=user)
    if len(userDB) == 0:
        userDB = User(username=user)
        userDB.save()
    else:
        userDB = userDB[0]  # raise some exception


def addTag(tag, user):
    try:
        userDB = User.objects.get(username=user)
    except Entry.DoesNotExist:
        pass
    tagDB = Tag.objects.filter(owner=userDB.id, tag=tag)
    if len(tagDB) == 0:
        tagDB = Tag(tag=tag, owner=userDB)
        tagDB.save()
    else:
        tagDB = tagDB[0]  # raise some exception


def addSentence(sentence, user, adding_date=timezone.now()):
    userDB = User.objects.get(username=user)
    sentenceDB = Sentence.objects.filter(owner=userDB.id,
            content=sentence)
    if len(sentenceDB) == 0:
        sentenceDB = Sentence(adding_date=timezone.now(), owner=userDB,
                              content=sentence)
        sentenceDB.save()
    else:
        sentenceDB = sentenceDB[0]  # raise some exception


def addEntries(entries):
    for entry in entries:
        addEntry(entry[0], entry[1], entry[2])


default_tag = '__no_tag__'


def search(tags, username):
    """
        @Description
            search for sentences marked with 
            @tags and created by @username
        @Args
            @tags <- list of string
            @username <- string
        @Returns
            @list of string representation of sentences
    """

    userDB = User.objects.get(username=username)
    if userDB is None:
        raise NotImplementedError()

    tagDBs = []
    for tag in tags:
        tagDBs.append(Tag.objects.filter(tag=tag,
                      owner=userDB.id)[0].id)

    tagSentenceDBs = \
        TagSentence.objects.filter(tag__in=tagDBs).values('sentence')

    sentences = []
    _tagSentenceDBs = \
        tagSentenceDBs.annotate(numSentence=Count('sentence'))

    for _tagSentenceDB in _tagSentenceDBs:
        if _tagSentenceDB['numSentence'] == len(tagDBs):
            sentences.append(Sentence.objects.filter(id=_tagSentenceDB['sentence'
                             ])[0])
    return [str(sentence) for sentence in sentences]