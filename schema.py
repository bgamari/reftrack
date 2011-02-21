from mongoengine import *

class User(Document):
        username = StringField(max_length=20)
        email = StringField(required=True)
        first_name = StringField()
        last_name = StringField()

class Ref(Document):
        doi = StringField()
        title = StringField()
        authors = ListField(StringField())
        year = IntField()
        path = StringField()
        filename = StringField()
        md5 = StringField(max_length=33)

class Tag(Document):
        name = StringField()
        user = ReferenceField(User)
        tagged = ReferenceField(Ref)

class JournalArticle(Ref):
        journal = StringField()
        full_journal = StringField()
        volume = StringField(max_length=20)
        issue = StringField(max_length=20)
        pages = StringField(max_length=20)

