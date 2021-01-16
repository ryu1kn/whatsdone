
export const awsSdkResponse = (response?: any) => ({
  promise: () => response instanceof Error ? Promise.reject(response) : Promise.resolve(response)
});
