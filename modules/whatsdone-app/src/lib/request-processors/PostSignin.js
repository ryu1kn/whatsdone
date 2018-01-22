
class PostSigninRequestProcessor {

  process(_request, _session) {
    return {
      statusCode: '200',
      headers: {
        'Content-Type': 'text/plain'
      }
    };
  }

}

module.exports = PostSigninRequestProcessor;
