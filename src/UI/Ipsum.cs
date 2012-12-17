using System;
using System.Text;

/// <summary>
/// Lorem Ipsum generator class for C#
/// </summary>
public class Ipsum
{
    private string[] words = new string[] { "\n", "\t", "feugiat", "nulla", "facilisis", "at", "vero", "eos", "et", "accusam",
    "et", "justo", "duo", "dolores", "et", "ea", "rebum", "stet", "clita", "kasd", "gubergren", "no", "sea",
    "takimata", "sanctus", "est", "lorem", "ipsum", "dolor", "sit", "amet", "lorem", "ipsum", "dolor", "sit",
    "amet", "consetetur", "sadipscing", "elitr", "sed", "diam", "nonumy", "eirmod", "tempor", "invidunt", "ut",
    "labore", "\n", "\t", "et", "dolore", "magna", "aliquyam", "erat", "sed", "diam", "voluptua", "at", "vero", "eos", "et",
    "accusam", "et", "justo", "duo", "dolores", "et", "ea", "rebum", "stet", "clita", "kasd", "gubergren", "no",
    "sea", "takimata", "sanctus", "est", "lorem", "\n", "\t", "ipsum", "dolor", "sit", "amet", "lorem", "ipsum", "dolor", "sit",
    "amet", "consetetur", "sadipscing", "elitr", "at", "accusam", "aliquyam", "diam", "diam", "dolore", "dolores",
    "duo", "eirmod", "eos", "erat", "et", "nonumy", "sed", "tempor", "et", "et", "invidunt", "justo", "labore",
    "stet", "clita", "ea", "et", "gubergren", "kasd", "magna", "no", "rebum", "sanctus", "sea", "sed", "takimata",
    "ut", "vero", "voluptua", "est", "lorem", "ipsum", "dolor", "sit", "amet", "lorem", "ipsum", "dolor", "sit",
    "amet", "consetetur", "sadipscing", "elitr", "\n", "\t", "sed", "diam", "nonumy",  "\n", "\t", "eirmod", "tempor", "invidunt", "ut",
    "labore", "et", "dolore", "magna", "aliquyam", "erat", "consetetur", "sadipscing", "elitr", "sed", "diam",
    "nonumy", "eirmod", "tempor", "invidunt", "ut", "labore", "et", "dolore", "magna", "aliquyam", "erat", "sed",
    "diam", "voluptua", "at", "vero", "eos", "et", "accusam", "et", "justo", "duo", "dolores", "et", "ea",
    "rebum", "stet", "clita", "\n", "\t", "kasd", "gubergren", "no", "sea", "takimata", "sanctus", "est", "lorem", "ipsum" };

    public string GetWords(int NumWords)
    {
        StringBuilder Result = new StringBuilder();


        Random random = new Random(DateTime.Now.Millisecond);

        for (int i = 0; i <= NumWords; i++)
        {
            Result.Append(" " + words[random.Next(words.Length - 1)]);
        }

        Result.Append(".");
        return Result.ToString();
    }
}
