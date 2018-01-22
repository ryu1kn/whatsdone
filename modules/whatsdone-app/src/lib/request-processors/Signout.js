
class SignoutRequestProcessor {

  process(_request, _session) {
    return {
      statusCode: '200'
    };
  }

}

module.exports = SignoutRequestProcessor;
