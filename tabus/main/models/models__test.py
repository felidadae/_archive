# -*- coding: utf-8 -*-
from django.utils import timezone
from django.test import TestCase

from django.utils import timezone

from main.models.models import *
import main.models.modelsOps as modelsOps
import main.models.testingUtils as testingUtils



class TestCase__models(TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test__addEntry(self):
        modelsOps.addEntries(testingUtils.test__entries)

        self.assertTrue(len(User.objects.all()) == 2)
        self.assertTrue(len(Sentence.objects.all()) == 4)
        self.assertEqual([str(db.username) for db in
                         User.objects.all()],
                         testingUtils.test__userFromEntries)

    def test__addUser(self):
        """
            check if we add twice users,
            we have only once added users
        """
        for i in range(2):
            for user in testingUtils.test__users:
                modelsOps.addUser(user)
        
        self.assertEqual([str(db.username) for db in
                         User.objects.all()].sort(),
                         testingUtils.test__users.sort())

    def test__addSentence(self):
        modelsOps.addEntries(testingUtils.test__entries)

        userDB = User.objects.get(username=testingUtils.test__users[0])
        for sentence in testingUtils.test__sentences:
            modelsOps.addSentence(
                sentence, userDB, adding_date=timezone.now())
        self.assertTrue(len(Sentence.objects.filter(
            content__in=testingUtils.test__sentences)))
        for sentence in testingUtils.test__sentences:
            modelsOps.addSentence(
                sentence, userDB, adding_date=timezone.now())
        self.assertTrue(
            len(Sentence.objects.filter(
                content__in=testingUtils.test__sentences)))

    def test__search(self):
        """
		1) test if finds sentences as providing
			all tags while adding the sentence
		2) check if giving to search only one tag
			which match many sentences, many
			sentences will be returned
		3)** what we are expecting when user doesn't have any sentences
		"""

        modelsOps.addEntries(testingUtils.test__entries)


        # (1)
        for test__entry in testingUtils.test__entries:
            (username, sentence, tags) = test__entry
            self.assertTrue(sentence in modelsOps.search(tags,
                            username))

        # (2)
        self.assertEqual(modelsOps.search(['wazne'], 'Agata'),
                         testingUtils.test__sentenes__agata_wazne)



			